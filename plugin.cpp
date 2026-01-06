#include "clang-tidy/ClangTidy.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "clang-tidy/ClangTidyOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/Token.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tidy;

static bool hasAnnotate(const Decl *D, llvm::StringRef Tag) {
    if(!D) return false;
    for(const Attr *A : D->attrs())
        if(const auto *Ann = dyn_cast<AnnotateAttr>(A))
            if(Ann->getAnnotation() == Tag) return true;
    return false;
}

static const Expr *ignore(const Expr *E) {
    return E ? E->IgnoreParenImpCasts() : nullptr;
}

static const Decl *designatedAtomicStorage(const Expr *E, llvm::StringRef TypeAnnot) {
    if(!E) return nullptr;
    E = ignore(E);

    // Array element access
    if(auto *ASE = dyn_cast<ArraySubscriptExpr>(E)) return designatedAtomicStorage(ASE->getBase(), TypeAnnot);

    // Deref: track through
    if(auto *UO = dyn_cast<UnaryOperator>(E))
        if(UO->getOpcode() == UO_Deref) return designatedAtomicStorage(UO->getSubExpr(), TypeAnnot);

    // Member
    if(auto *ME = dyn_cast<MemberExpr>(E)) {
        if(auto *FD = dyn_cast<FieldDecl>(ME->getMemberDecl())) return hasAnnotate(FD, TypeAnnot) ? FD : nullptr;
        return nullptr;
    }

    // Plain decl ref
    if(auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        if(auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) return hasAnnotate(VD, TypeAnnot) ? VD : nullptr;
    }

    return nullptr;
}

static bool addrFlowsToAllowedCall(ASTContext &Ctx, const UnaryOperator *AddrOf, llvm::Regex &AllowedFns, llvm::StringRef ParamAnnot) {
    const Stmt *Cur = AddrOf;
    for(unsigned hops = 0; hops < 128 && Cur; ++hops) {
        auto Parents = Ctx.getParentMapContext().getParents(*Cur);
        if(Parents.empty()) break;

        const Stmt *NextS = nullptr;
        for(const auto &P : Parents) {
            if(const auto *S = P.get<Stmt>()) {
                if(isa<ImplicitCastExpr>(S) || isa<ParenExpr>(S) || isa<CStyleCastExpr>(S)) {
                    NextS = S;
                    break;
                }

                if(const auto *Call = dyn_cast<CallExpr>(S)) {
                    const FunctionDecl *FD = Call->getDirectCallee();
                    if(FD && AllowedFns.match(FD->getNameAsString())) return true;

                    if(!FD) return false;

                    for(unsigned i = 0, e = Call->getNumArgs(); i != e; ++i) {
                        if(Call->getArg(i)->IgnoreParenImpCasts() == Cur) {
                            if(i < FD->getNumParams()) {
                                const ParmVarDecl *PVD = FD->getParamDecl(i);
                                if(hasAnnotate(PVD, ParamAnnot)) return true;
                            }
                            break;
                        }
                    }
                    return false;
                }

                if(const auto *AE = dyn_cast<AtomicExpr>(S)) {
                    (void) AE;
                    return true;
                }
            }
        }
        Cur = NextS;
    }
    return false;
}

class AtomicOnlyVisitor : public RecursiveASTVisitor<AtomicOnlyVisitor> {
  public:
    AtomicOnlyVisitor(ASTContext &C, DiagnosticsEngine &DE, llvm::StringRef Allowed, llvm::StringRef ParamAnn, llvm::StringRef TypeAnn) : Ctx(C), Diags(DE), AllowedFns(Allowed), ParamAnnot(ParamAnn), TypeAnnot(TypeAnn) {
        DWrite = Diags.getCustomDiagID(DiagnosticsEngine::Error, "storage '%0' is marked atomic; direct write is forbidden (use __atomic_* or a whitelisted wrapper)");
        DIncDec = Diags.getCustomDiagID(DiagnosticsEngine::Error, "storage '%0' is marked atomic; ++/-- are forbidden (use atomic fetch-add/sub)");
        DRead = Diags.getCustomDiagID(DiagnosticsEngine::Error, "storage '%0' is marked atomic; raw read is forbidden (use __atomic_load_n or a whitelisted wrapper)");
        DAddr = Diags.getCustomDiagID(DiagnosticsEngine::Error, "taking address of '%0' is only allowed for atomic builtins or whitelisted wrappers");
    }

    bool VisitBinaryOperator(BinaryOperator *BO) {
        if(!BO->isAssignmentOp()) return true;
        if(const Decl *D = designatedAtomicStorage(BO->getLHS(), TypeAnnot)) {
            report(BO->getOperatorLoc(), DWrite, D);
        }
        return true;
    }

    bool VisitUnaryOperator(UnaryOperator *UO) {
        switch(UO->getOpcode()) {
            case UO_PreInc:
            case UO_PostInc:
            case UO_PreDec:
            case UO_PostDec:
                if(const Decl *D = designatedAtomicStorage(UO->getSubExpr(), TypeAnnot)) report(UO->getOperatorLoc(), DIncDec, D);
                break;
            case UO_AddrOf:
                if(const Decl *D = designatedAtomicStorage(UO->getSubExpr(), TypeAnnot)) {
                    if(!addrFlowsToAllowedCall(Ctx, UO, AllowedFns, ParamAnnot)) report(UO->getOperatorLoc(), DAddr, D);
                }
                break;
            default: break;
        }
        return true;
    }

    bool VisitImplicitCastExpr(ImplicitCastExpr *ICE) {
        if(ICE->getCastKind() != CK_LValueToRValue) return true;
        if(const Decl *D = designatedAtomicStorage(ICE->getSubExpr(), TypeAnnot)) report(ICE->getExprLoc(), DRead, D);
        return true;
    }

  private:
    ASTContext &Ctx;
    DiagnosticsEngine &Diags;
    llvm::Regex AllowedFns;
    std::string ParamAnnot;
    std::string TypeAnnot;

    unsigned DWrite{}, DIncDec{}, DRead{}, DAddr{};

    void report(SourceLocation Loc, unsigned ID, const Decl *D) {
        if(const auto *FD = dyn_cast<FieldDecl>(D))
            Diags.Report(Loc, ID) << FD->getName();
        else if(const auto *VD = dyn_cast<VarDecl>(D))
            Diags.Report(Loc, ID) << VD->getName();
        else
            Diags.Report(Loc, ID) << "<atomic>";
    }
};

namespace elysium {

class AtomicOnlyCheck : public ClangTidyCheck {
  public:
    AtomicOnlyCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
        Allowed = Options.get("AllowedFns", "^$");
        ParamAnn = Options.get("ParamAnnot", "atomic_only_param");
        TypeAnn = Options.get("TypeAnnot", "atomic_only");
    }

    void storeOptions(ClangTidyOptions::OptionMap &Opts) override {
        Options.store(Opts, "AllowedFns", Allowed);
        Options.store(Opts, "ParamAnnot", ParamAnn);
        Options.store(Opts, "TypeAnnot", TypeAnn);
    }

    void registerMatchers(MatchFinder *Finder) override {
        Finder->addMatcher(translationUnitDecl().bind("tu"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        const auto *TU = Result.Nodes.getNodeAs<TranslationUnitDecl>("tu");

        if(!TU) return;

        AtomicOnlyVisitor V(*Result.Context, Result.Context->getDiagnostics(), Allowed, ParamAnn, TypeAnn);
        V.TraverseDecl(const_cast<TranslationUnitDecl *>(TU));
    }

  private:
    std::string Allowed, ParamAnn, TypeAnn;
};

class GlobalPrefixCheck : public ClangTidyCheck {
  public:
    GlobalPrefixCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
        Prefix = Options.get("Prefix", "g_");
    }

    void storeOptions(ClangTidyOptions::OptionMap &Opts) override {
        Options.store(Opts, "Prefix", Prefix);
    }

    void registerMatchers(MatchFinder *Finder) override {
        Finder->addMatcher(varDecl(isDefinition(), unless(anyOf(isImplicit(), isExpansionInSystemHeader()))).bind("var"), this);
        Finder->addMatcher(namedDecl(unless(anyOf(varDecl(), isImplicit(), isExpansionInSystemHeader()))).bind("named"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        if(Prefix.empty()) return;

        if(const auto *VD = Result.Nodes.getNodeAs<VarDecl>("var")) {
            if(!VD->getIdentifier()) return;

            StringRef Name = VD->getName();
            bool HasPrefix = Name.starts_with(Prefix);
            bool IsGlobal = VD->isFileVarDecl();

            if(IsGlobal) {
                if(!HasPrefix) diag(VD->getLocation(), "global variable '%0' must be prefixed with %1") << Name << Prefix;
                return;
            }

            if(HasPrefix) diag(VD->getLocation(), "only global variables may use the %0 prefix; '%1' is not global") << Prefix << Name;
            return;
        }

        const auto *ND = Result.Nodes.getNodeAs<NamedDecl>("named");
        if(!ND) return;
        if(!ND->getIdentifier()) return;

        StringRef Name = ND->getName();
        if(Name.starts_with(Prefix)) diag(ND->getLocation(), "only global variables may use the %0 prefix; '%1' is not a global variable") << Prefix << Name;
    }

  private:
    std::string Prefix;
};

class HeaderNamespaceCheck : public ClangTidyCheck {
  public:
    HeaderNamespaceCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
        HeaderSuffixes = Options.get("HeaderSuffixes", ".h;.hh;.hpp");
        parseSuffixes();

        SpecialPrefixSpec = Options.get("SpecialDirectoryPrefixes", "");
        parseSpecialPrefixes();

        ExtraPrefixSpec = Options.get("SpecialGlobalPrefixes", "");
        parseExtraPrefixes();

        loadCaseOptions(Ctx);

        GlobalPrefix = "";
        if(!Ctx || !Ctx->isCheckEnabled("elysium-global-prefix")) return;

        GlobalPrefix = "g_";
        const auto &AllOpts = Ctx->getOptions().CheckOptions;
        auto It = AllOpts.find("elysium-global-prefix.Prefix");
        if(It != AllOpts.end()) GlobalPrefix = It->second.Value;
    }

    void storeOptions(ClangTidyOptions::OptionMap &Opts) override {
        Options.store(Opts, "HeaderSuffixes", HeaderSuffixes);
        Options.store(Opts, "SpecialDirectoryPrefixes", SpecialPrefixSpec);
        Options.store(Opts, "SpecialGlobalPrefixes", ExtraPrefixSpec);
    }

    void registerMatchers(MatchFinder *Finder) override {
        Finder->addMatcher(namedDecl(unless(isImplicit())).bind("named"), this);
    }

    void registerPPCallbacks(const SourceManager &SM, Preprocessor *PP, Preprocessor *) override {
        if(!PP || SuffixList.empty()) return;
        PP->addPPCallbacks(std::make_unique<NamespacePPCallbacks>(*this, SM));
    }

    void check(const MatchFinder::MatchResult &Result) override {
        if(SuffixList.empty()) return;

        const auto *ND = Result.Nodes.getNodeAs<NamedDecl>("named");
        if(!ND) return;
        if(!ND->getIdentifier()) return;
        if(!ND->isFirstDecl()) return;

        auto Kind = classifyDecl(ND);
        if(!Kind) return;
        if(!isGloballyExposed(ND, *Kind)) return;
        if(const auto *TD = dyn_cast<TagDecl>(ND))
            if(!TD->isCompleteDefinition()) return;

        HeaderInfo Info;
        if(!gatherHeaderInfo(ND->getLocation(), *Result.SourceManager, Info)) return;

        enforcePrefix(ND->getName(), *Kind, ND->getLocation(), Info);
    }

  private:
    enum class NameKind {
        Variable,
        Function,
        Struct,
        Union,
        Enum,
        EnumCase,
        Typedef,
        Macro,
        Count
    };

    enum class NameCase {
        Lower,
        Upper
    };

    struct SpecialPrefixRule {
        std::string DirectorySpec;
        std::string MatchFragment;
        std::string PrefixLower;
        std::string PrefixUpper;
    };

    struct HeaderInfo {
        std::string HeaderName;
        std::string StemLower;
        std::string StemUpper;
        const SpecialPrefixRule *DirRule{ nullptr };
        FileID File;
    };

    struct MacroCandidate {
        std::string Name;
        HeaderInfo Info;
        SourceLocation Loc;
        bool UndefinedInHeader{ false };
    };

    struct ExtraPrefix {
        std::string Raw;
        std::string Lower;
        std::string Upper;
    };

    class NamespacePPCallbacks : public PPCallbacks {
      public:
        NamespacePPCallbacks(HeaderNamespaceCheck &Check, const SourceManager &SM) : TheCheck(Check), SM(SM) {}

        void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) override {
            if(!MD) return;
            const MacroInfo *MI = MD->getMacroInfo();
            if(!MI) return;
            SourceLocation Loc = MI->getDefinitionLoc();
            if(Loc.isInvalid()) return;
            if(const IdentifierInfo *II = MacroNameTok.getIdentifierInfo()) {
                TheCheck.noteMacroDefinition(II, Loc, SM);
            }
        }

        void MacroUndefined(const Token &MacroNameTok, const MacroDefinition &, const MacroDirective *Undef) override {
            if(!Undef) return;
            SourceLocation Loc = Undef->getLocation();
            if(Loc.isInvalid()) return;
            if(const IdentifierInfo *II = MacroNameTok.getIdentifierInfo()) {
                TheCheck.noteMacroUndefined(II, Loc, SM);
            }
        }

        void EndOfMainFile() override {
            TheCheck.flushMacroDefinitions();
        }

      private:
        HeaderNamespaceCheck &TheCheck;
        const SourceManager &SM;
    };

    std::string HeaderSuffixes;
    std::string SpecialPrefixSpec;
    std::string ExtraPrefixSpec;
    std::string GlobalPrefix;
    std::vector<std::string> SuffixList;
    std::array<NameCase, static_cast<size_t>(NameKind::Count)> CasePrefs{};
    std::vector<SpecialPrefixRule> SpecialPrefixRules;
    std::vector<MacroCandidate> MacroCandidates;
    std::vector<ExtraPrefix> ExtraPrefixes;

    static size_t kindIndex(NameKind Kind) {
        return static_cast<size_t>(Kind);
    }

    void parseSuffixes() {
        SuffixList.clear();

        StringRef Remaining(HeaderSuffixes);
        while(!Remaining.empty()) {
            size_t Pos = Remaining.find_first_of(";,");
            StringRef Entry = Pos == StringRef::npos ? Remaining : Remaining.take_front(Pos);
            Remaining = Pos == StringRef::npos ? StringRef() : Remaining.drop_front(Pos + 1);
            Entry = Entry.trim();
            if(Entry.empty()) continue;

            std::string Normalized = Entry.str();
            if(Normalized.front() != '.') Normalized.insert(Normalized.begin(), '.');
            SuffixList.push_back(Normalized);
        }

        if(SuffixList.empty()) SuffixList.emplace_back(".h");
    }

    void noteMacroDefinition(const IdentifierInfo *II, SourceLocation Loc, const SourceManager &SM) {
        if(!II) return;
        HeaderInfo Info;
        if(!gatherHeaderInfo(Loc, SM, Info)) return;
        MacroCandidate MC;
        MC.Name = II->getName().str();
        MC.Info = Info;
        MC.Loc = Loc;
        MacroCandidates.push_back(std::move(MC));
    }

    void noteMacroUndefined(const IdentifierInfo *II, SourceLocation Loc, const SourceManager &SM) {
        if(!II) return;
        HeaderInfo Info;
        if(!gatherHeaderInfo(Loc, SM, Info)) return;
        for(auto It = MacroCandidates.rbegin(); It != MacroCandidates.rend(); ++It) {
            if(It->UndefinedInHeader) continue;
            if(It->Name != II->getName()) continue;
            if(It->Info.File != Info.File) continue;
            It->UndefinedInHeader = true;
            break;
        }
    }

    void flushMacroDefinitions() {
        for(const auto &MC : MacroCandidates) {
            if(MC.UndefinedInHeader) continue;
            enforcePrefix(MC.Name, NameKind::Macro, MC.Loc, MC.Info);
        }
        MacroCandidates.clear();
    }

    static std::string normalizePathFragment(StringRef Path) {
        std::string S = Path.str();
        for(char &C : S)
            if(C == '\\') C = '/';
        while(S.size() > 1 && S.back() == '/') S.pop_back();
        if(!S.empty() && S.front() != '/') S.insert(S.begin(), '/');
        return S;
    }

    static std::string normalizeFullPath(StringRef Path) {
        std::string S = Path.str();
        for(char &C : S)
            if(C == '\\') C = '/';
        if(!S.empty() && S.front() != '/') S.insert(S.begin(), '/');
        return S;
    }

    void parseSpecialPrefixes() {
        SpecialPrefixRules.clear();
        StringRef Remaining(SpecialPrefixSpec);
        while(!Remaining.empty()) {
            size_t Pos = Remaining.find_first_of(";,");
            StringRef Entry = Pos == StringRef::npos ? Remaining : Remaining.take_front(Pos);
            Remaining = Pos == StringRef::npos ? StringRef() : Remaining.drop_front(Pos + 1);
            Entry = Entry.trim();
            if(Entry.empty()) continue;
            size_t Sep = Entry.find(':');
            if(Sep == StringRef::npos) continue;
            StringRef Dir = Entry.take_front(Sep).trim();
            StringRef Prefix = Entry.drop_front(Sep + 1).trim();
            if(Dir.empty() || Prefix.empty()) continue;
            SpecialPrefixRule Rule;
            Rule.DirectorySpec = Dir.str();
            Rule.MatchFragment = normalizePathFragment(Dir);
            if(Rule.MatchFragment.empty()) continue;
            Rule.PrefixLower = Prefix.lower();
            Rule.PrefixUpper = Rule.PrefixLower;
            for(char &C : Rule.PrefixUpper) C = llvm::toUpper(C);
            SpecialPrefixRules.push_back(std::move(Rule));
        }
    }

    void parseExtraPrefixes() {
        ExtraPrefixes.clear();
        StringRef Remaining(ExtraPrefixSpec);
        while(!Remaining.empty()) {
            size_t Pos = Remaining.find_first_of(";,");
            StringRef Entry = Pos == StringRef::npos ? Remaining : Remaining.take_front(Pos);
            Remaining = Pos == StringRef::npos ? StringRef() : Remaining.drop_front(Pos + 1);
            Entry = Entry.trim();
            if(Entry.empty()) continue;
            ExtraPrefix Pfx;
            Pfx.Raw = Entry.str();
            Pfx.Lower = Entry.lower();
            Pfx.Upper = Pfx.Lower;
            for(char &C : Pfx.Upper) C = llvm::toUpper(C);
            ExtraPrefixes.push_back(std::move(Pfx));
        }
    }

    void loadCaseOptions(ClangTidyContext *Ctx) {
        for(size_t I = 0; I < CasePrefs.size(); ++I) CasePrefs[I] = determineCasePreference(static_cast<NameKind>(I), Ctx);
    }

    NameCase determineCasePreference(NameKind Kind, ClangTidyContext *Ctx) const {
        if(Ctx) {
            const auto &AllOpts = Ctx->getOptions().CheckOptions;
            if(const char *Key = readabilityCaseKey(Kind); Key && *Key) {
                auto It = AllOpts.find(Key);
                if(It != AllOpts.end())
                    if(auto Parsed = caseFromReadabilityValue(It->second.Value)) return *Parsed;
            }
        }
        return NameCase::Lower;
    }

    static std::optional<NameCase> caseFromReadabilityValue(StringRef Value) {
        if(Value.empty()) return std::nullopt;
        std::string Lower = Value.lower();
        return llvm::StringSwitch<std::optional<NameCase>>(Lower)
            .Case("lower_case", NameCase::Lower)
            .Case("snake_case", NameCase::Lower)
            .Case("lower", NameCase::Lower)
            .Case("upper_case", NameCase::Upper)
            .Case("screaming_snake_case", NameCase::Upper)
            .Case("upper", NameCase::Upper)
            .Default(std::nullopt);
    }


    static const char *readabilityCaseKey(NameKind Kind) {
        switch(Kind) {
            case NameKind::Variable: return "readability-identifier-naming.VariableCase";
            case NameKind::Function: return "readability-identifier-naming.FunctionCase";
            case NameKind::Struct:   return "readability-identifier-naming.StructCase";
            case NameKind::Union:    return "readability-identifier-naming.UnionCase";
            case NameKind::Enum:     return "readability-identifier-naming.EnumCase";
            case NameKind::EnumCase: return "readability-identifier-naming.EnumConstantCase";
            case NameKind::Typedef:  return "readability-identifier-naming.TypedefCase";
            case NameKind::Macro:    return "readability-identifier-naming.MacroDefinitionCase";
            case NameKind::Count:    break;
        }
        return nullptr;
    }

    const SpecialPrefixRule *determinePrefixRule(StringRef FilePath) const {
        if(SpecialPrefixRules.empty()) return nullptr;
        std::string Normalized = normalizeFullPath(FilePath);
        const SpecialPrefixRule *Best = nullptr;
        for(const auto &Rule : SpecialPrefixRules) {
            if(Rule.MatchFragment.empty()) continue;
            if(pathContainsDirectory(Normalized, Rule.MatchFragment))
                if(!Best || Rule.MatchFragment.size() > Best->MatchFragment.size()) Best = &Rule;
        }
        return Best;
    }

    static bool pathContainsDirectory(StringRef Path, StringRef Fragment) {
        if(Fragment.empty()) return false;
        size_t Pos = Path.find(Fragment);
        while(Pos != StringRef::npos) {
            bool StartOk = true;
            if(Fragment.front() != '/') StartOk = (Pos == 0) || Path[Pos - 1] == '/';
            size_t After = Pos + Fragment.size();
            bool EndOk = (After == Path.size()) || Path[After] == '/';
            if(StartOk && EndOk) return true;
            Pos = Path.find(Fragment, Pos + 1);
        }
        return false;
    }

    bool allowGlobalPrefixedVariable(StringRef Name, StringRef Stem, StringRef Expected, StringRef ExpectedPlural) const {
        if(GlobalPrefix.empty()) return false;
        if(!Name.starts_with(GlobalPrefix)) return false;
        StringRef Rest = Name.drop_front(GlobalPrefix.size());
        if(Rest == Stem) return true;
        if(Rest.starts_with(Expected)) return true;
        if(Rest.starts_with(ExpectedPlural)) return true;
        return false;
    }

    bool isHeaderFile(StringRef Path) const {
        StringRef Ext = llvm::sys::path::extension(Path);
        if(Ext.empty()) return false;
        for(const auto &S : SuffixList)
            if(Ext == S) return true;
        return false;
    }

    std::optional<NameKind> classifyDecl(const NamedDecl *ND) const {
        if(isa<VarDecl>(ND)) return NameKind::Variable;
        if(isa<FunctionDecl>(ND)) return NameKind::Function;
        if(const auto *RD = dyn_cast<RecordDecl>(ND)) {
            if(RD->isStruct()) return NameKind::Struct;
            if(RD->isUnion()) return NameKind::Union;
        }
        if(isa<EnumDecl>(ND)) return NameKind::Enum;
        if(isa<EnumConstantDecl>(ND)) return NameKind::EnumCase;
        if(isa<TypedefNameDecl>(ND)) return NameKind::Typedef;
        return std::nullopt;
    }

    bool isGlobalRecord(const RecordDecl *RD) const {
        if(!RD) return false;
        const DeclContext *Ctx = RD->getDeclContext();
        while(Ctx && !Ctx->isFileContext()) {
            const auto *ParentDecl = dyn_cast<Decl>(Ctx);
            Ctx = ParentDecl ? ParentDecl->getDeclContext() : nullptr;
        }
        return Ctx && Ctx->isFileContext();
    }

    bool isGlobalFunction(const FunctionDecl *FD) const {
        if(!FD) return false;
        const DeclContext *Ctx = FD->getDeclContext();
        while(Ctx && !Ctx->isFileContext()) {
            const auto *ParentDecl = dyn_cast<Decl>(Ctx);
            Ctx = ParentDecl ? ParentDecl->getDeclContext() : nullptr;
        }
        if(!Ctx || !Ctx->isFileContext()) return false;
        return FD->isExternallyVisible();
    }

    bool isGloballyExposed(const NamedDecl *ND, NameKind Kind) const {
        switch(Kind) {
            case NameKind::Variable: {
                const auto *VD = cast<VarDecl>(ND);
                return VD->isFileVarDecl();
            }
            case NameKind::Function: return isGlobalFunction(cast<FunctionDecl>(ND));
            case NameKind::Struct:
            case NameKind::Union:    return isGlobalRecord(cast<RecordDecl>(ND));
            case NameKind::Enum:     {
                const auto *ED = cast<EnumDecl>(ND);
                return ED->getDeclContext()->isFileContext();
            }
            case NameKind::EnumCase: {
                const auto *ECD = cast<EnumConstantDecl>(ND);
                const auto *ED = dyn_cast<EnumDecl>(ECD->getDeclContext());
                return ED && ED->getDeclContext()->isFileContext();
            }
            case NameKind::Typedef: return ND->getDeclContext()->isFileContext();
            case NameKind::Macro:
            case NameKind::Count:   break;
        }
        return false;
    }

    bool gatherHeaderInfo(SourceLocation Loc, const SourceManager &SM, HeaderInfo &Info) const {
        if(Loc.isInvalid()) return false;
        Loc = SM.getFileLoc(Loc);
        if(Loc.isInvalid()) return false;
        if(SM.isInSystemHeader(Loc)) return false;

        StringRef FilePath = SM.getFilename(Loc);
        if(FilePath.empty()) return false;
        if(!isHeaderFile(FilePath)) return false;

        StringRef HeaderName = llvm::sys::path::filename(FilePath);
        StringRef Stem = llvm::sys::path::stem(HeaderName);
        if(Stem.empty()) return false;

        Info.HeaderName = HeaderName.str();
        Info.StemLower = Stem.lower();
        Info.StemUpper = Stem.upper();
        Info.DirRule = determinePrefixRule(FilePath);
        Info.File = SM.getFileID(Loc);
        return true;
    }

    void enforcePrefix(StringRef DeclName, NameKind Kind, SourceLocation Loc, const HeaderInfo &Info) {
        NameCase Pref = CasePrefs[kindIndex(Kind)];
        const std::string &Stem = Pref == NameCase::Upper ? Info.StemUpper : Info.StemLower;
        std::string CombinedStem;
        if(const SpecialPrefixRule *Rule = Info.DirRule) CombinedStem += (Pref == NameCase::Upper ? Rule->PrefixUpper : Rule->PrefixLower);
        CombinedStem += Stem;

        std::vector<std::string> Roots;
        Roots.push_back(CombinedStem);
        for(const auto &Extra : ExtraPrefixes) {
            const std::string &Prefix = Pref == NameCase::Upper ? Extra.Upper : Extra.Lower;
            std::string Root = Prefix + CombinedStem;
            Roots.push_back(std::move(Root));
        }

        for(const std::string &RootStr : Roots) {
            if(RootStr.empty()) continue;
            std::string Expected = RootStr + "_";
            std::string ExpectedPlural = RootStr + "s_";
            StringRef RootRef(RootStr);
            StringRef ExpectedRef(Expected);
            StringRef ExpectedPluralRef(ExpectedPlural);

            if(DeclName == RootRef) return;
            if(DeclName.starts_with(ExpectedRef)) return;
            if(DeclName.starts_with(ExpectedPluralRef)) return;
            if(Kind == NameKind::Variable && allowGlobalPrefixedVariable(DeclName, RootRef, ExpectedRef, ExpectedPluralRef)) return;
        }

        diag(Loc, "%0 '%1' in header '%2' must be prefixed with '%3'") << kindDisplayName(Kind) << DeclName << Info.HeaderName << (CombinedStem + "_");
    }

    static const char *kindDisplayName(NameKind Kind) {
        switch(Kind) {
            case NameKind::Variable: return "variable";
            case NameKind::Function: return "function";
            case NameKind::Struct:   return "struct";
            case NameKind::Union:    return "union";
            case NameKind::Enum:     return "enum";
            case NameKind::EnumCase: return "enum constant";
            case NameKind::Typedef:  return "typedef";
            case NameKind::Macro:    return "macro";
            case NameKind::Count:    return "declaration";
        }
        return "declaration";
    }
};

class ElysiumModule : public ClangTidyModule {
  public:
    void addCheckFactories(ClangTidyCheckFactories &Factories) override {
        Factories.registerCheck<AtomicOnlyCheck>("elysium-atomic-checks");
        Factories.registerCheck<GlobalPrefixCheck>("elysium-global-prefix");
        Factories.registerCheck<HeaderNamespaceCheck>("elysium-header-namespace");
    }
};

} // namespace elysium

static ClangTidyModuleRegistry::Add<elysium::ElysiumModule> X("elysium-tidy", "Custom clang-tidy checks for Elysium.");
