#include "HeaderNamespaceCheck.h"

#include "clang-tidy/ClangTidyOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/Token.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Path.h"

#include <memory>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tidy;

namespace elysium {

HeaderNamespaceCheck::NamespacePPCallbacks::NamespacePPCallbacks(HeaderNamespaceCheck &Check, const SourceManager &SM) : TheCheck(Check), SM(SM) {}

void HeaderNamespaceCheck::NamespacePPCallbacks::MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) {
    if(!MD) return;
    const MacroInfo *MI = MD->getMacroInfo();
    if(!MI) return;
    SourceLocation Loc = MI->getDefinitionLoc();
    if(Loc.isInvalid()) return;
    if(const IdentifierInfo *II = MacroNameTok.getIdentifierInfo()) {
        TheCheck.noteMacroDefinition(II, Loc, SM);
    }
}

void HeaderNamespaceCheck::NamespacePPCallbacks::MacroUndefined(const Token &MacroNameTok, const MacroDefinition &, const MacroDirective *Undef) {
    if(!Undef) return;
    SourceLocation Loc = Undef->getLocation();
    if(Loc.isInvalid()) return;
    if(const IdentifierInfo *II = MacroNameTok.getIdentifierInfo()) {
        TheCheck.noteMacroUndefined(II, Loc, SM);
    }
}

void HeaderNamespaceCheck::NamespacePPCallbacks::EndOfMainFile() {
    TheCheck.flushMacroDefinitions();
}

HeaderNamespaceCheck::HeaderNamespaceCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
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

void HeaderNamespaceCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "HeaderSuffixes", HeaderSuffixes);
    Options.store(Opts, "SpecialDirectoryPrefixes", SpecialPrefixSpec);
    Options.store(Opts, "SpecialGlobalPrefixes", ExtraPrefixSpec);
}

void HeaderNamespaceCheck::registerMatchers(MatchFinder *Finder) {
    Finder->addMatcher(namedDecl(unless(isImplicit())).bind("named"), this);
}

void HeaderNamespaceCheck::registerPPCallbacks(const SourceManager &SM, Preprocessor *PP, Preprocessor *) {
    if(!PP || SuffixList.empty()) return;
    PP->addPPCallbacks(std::make_unique<NamespacePPCallbacks>(*this, SM));
}

void HeaderNamespaceCheck::check(const MatchFinder::MatchResult &Result) {
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

size_t HeaderNamespaceCheck::kindIndex(NameKind Kind) {
    return static_cast<size_t>(Kind);
}

void HeaderNamespaceCheck::parseSuffixes() {
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

void HeaderNamespaceCheck::noteMacroDefinition(const IdentifierInfo *II, SourceLocation Loc, const SourceManager &SM) {
    if(!II) return;
    HeaderInfo Info;
    if(!gatherHeaderInfo(Loc, SM, Info)) return;
    MacroCandidate MC;
    MC.Name = II->getName().str();
    MC.Info = Info;
    MC.Loc = Loc;
    MacroCandidates.push_back(std::move(MC));
}

void HeaderNamespaceCheck::noteMacroUndefined(const IdentifierInfo *II, SourceLocation Loc, const SourceManager &SM) {
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

void HeaderNamespaceCheck::flushMacroDefinitions() {
    for(const auto &MC : MacroCandidates) {
        if(MC.UndefinedInHeader) continue;
        enforcePrefix(MC.Name, NameKind::Macro, MC.Loc, MC.Info);
    }
    MacroCandidates.clear();
}

std::string HeaderNamespaceCheck::normalizePathFragment(StringRef Path) {
    std::string S = Path.str();
    for(char &C : S)
        if(C == '\\') C = '/';
    while(S.size() > 1 && S.back() == '/') S.pop_back();
    if(!S.empty() && S.front() != '/') S.insert(S.begin(), '/');
    return S;
}

std::string HeaderNamespaceCheck::normalizeFullPath(StringRef Path) {
    std::string S = Path.str();
    for(char &C : S)
        if(C == '\\') C = '/';
    if(!S.empty() && S.front() != '/') S.insert(S.begin(), '/');
    return S;
}

void HeaderNamespaceCheck::parseSpecialPrefixes() {
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

void HeaderNamespaceCheck::parseExtraPrefixes() {
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

void HeaderNamespaceCheck::loadCaseOptions(ClangTidyContext *Ctx) {
    for(size_t I = 0; I < CasePrefs.size(); ++I) CasePrefs[I] = determineCasePreference(static_cast<NameKind>(I), Ctx);
}

HeaderNamespaceCheck::NameCase HeaderNamespaceCheck::determineCasePreference(NameKind Kind, ClangTidyContext *Ctx) const {
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

std::optional<HeaderNamespaceCheck::NameCase> HeaderNamespaceCheck::caseFromReadabilityValue(StringRef Value) {
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

const char *HeaderNamespaceCheck::readabilityCaseKey(NameKind Kind) {
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

const HeaderNamespaceCheck::SpecialPrefixRule *HeaderNamespaceCheck::determinePrefixRule(StringRef FilePath) const {
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

bool HeaderNamespaceCheck::pathContainsDirectory(StringRef Path, StringRef Fragment) {
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

bool HeaderNamespaceCheck::allowGlobalPrefixedVariable(StringRef Name, StringRef Stem, StringRef Expected, StringRef ExpectedPlural) const {
    if(GlobalPrefix.empty()) return false;
    if(!Name.starts_with(GlobalPrefix)) return false;
    StringRef Rest = Name.drop_front(GlobalPrefix.size());
    if(Rest == Stem) return true;
    if(Rest.starts_with(Expected)) return true;
    if(Rest.starts_with(ExpectedPlural)) return true;
    return false;
}

bool HeaderNamespaceCheck::isHeaderFile(StringRef Path) const {
    StringRef Ext = llvm::sys::path::extension(Path);
    if(Ext.empty()) return false;
    for(const auto &S : SuffixList)
        if(Ext == S) return true;
    return false;
}

std::optional<HeaderNamespaceCheck::NameKind> HeaderNamespaceCheck::classifyDecl(const NamedDecl *ND) const {
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

bool HeaderNamespaceCheck::isGlobalRecord(const RecordDecl *RD) const {
    if(!RD) return false;
    const DeclContext *Ctx = RD->getDeclContext();
    while(Ctx && !Ctx->isFileContext()) {
        const auto *ParentDecl = dyn_cast<Decl>(Ctx);
        Ctx = ParentDecl ? ParentDecl->getDeclContext() : nullptr;
    }
    return Ctx && Ctx->isFileContext();
}

bool HeaderNamespaceCheck::isGlobalFunction(const FunctionDecl *FD) const {
    if(!FD) return false;
    const DeclContext *Ctx = FD->getDeclContext();
    while(Ctx && !Ctx->isFileContext()) {
        const auto *ParentDecl = dyn_cast<Decl>(Ctx);
        Ctx = ParentDecl ? ParentDecl->getDeclContext() : nullptr;
    }
    if(!Ctx || !Ctx->isFileContext()) return false;
    return FD->isExternallyVisible();
}

bool HeaderNamespaceCheck::isGloballyExposed(const NamedDecl *ND, NameKind Kind) const {
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

bool HeaderNamespaceCheck::gatherHeaderInfo(SourceLocation Loc, const SourceManager &SM, HeaderInfo &Info) const {
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

void HeaderNamespaceCheck::enforcePrefix(StringRef DeclName, NameKind Kind, SourceLocation Loc, const HeaderInfo &Info) {
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

const char *HeaderNamespaceCheck::kindDisplayName(NameKind Kind) {
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

} // namespace elysium
