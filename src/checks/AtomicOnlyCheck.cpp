#include "AtomicOnlyCheck.h"

#include "clang-tidy/ClangTidyOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/Support/Regex.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tidy;

namespace {

bool hasAnnotate(const Decl *D, llvm::StringRef Tag) {
    if(!D) return false;
    for(const Attr *A : D->attrs())
        if(const auto *Ann = dyn_cast<AnnotateAttr>(A))
            if(Ann->getAnnotation() == Tag) return true;
    return false;
}

const Expr *ignore(const Expr *E) {
    return E ? E->IgnoreParenImpCasts() : nullptr;
}

const Decl *designatedAtomicStorage(const Expr *E, llvm::StringRef TypeAnnot) {
    if(!E) return nullptr;
    E = ignore(E);

    if(auto *ASE = dyn_cast<ArraySubscriptExpr>(E)) return designatedAtomicStorage(ASE->getBase(), TypeAnnot);

    if(auto *UO = dyn_cast<UnaryOperator>(E))
        if(UO->getOpcode() == UO_Deref) return designatedAtomicStorage(UO->getSubExpr(), TypeAnnot);

    if(auto *ME = dyn_cast<MemberExpr>(E)) {
        if(auto *FD = dyn_cast<FieldDecl>(ME->getMemberDecl())) return hasAnnotate(FD, TypeAnnot) ? FD : nullptr;
        return nullptr;
    }

    if(auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        if(auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) return hasAnnotate(VD, TypeAnnot) ? VD : nullptr;
    }

    return nullptr;
}

bool addrFlowsToAllowedCall(ASTContext &Ctx, const UnaryOperator *AddrOf, llvm::Regex &AllowedFns, llvm::StringRef ParamAnnot) {
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
        if(const Decl *D = designatedAtomicStorage(BO->getLHS(), TypeAnnot)) { report(BO->getOperatorLoc(), DWrite, D); }
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

} // namespace

namespace elysium {

AtomicOnlyCheck::AtomicOnlyCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
    Allowed = Options.get("AllowedFns", "^$");
    ParamAnn = Options.get("ParamAnnot", "atomic_only_param");
    TypeAnn = Options.get("TypeAnnot", "atomic_only");
}

void AtomicOnlyCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "AllowedFns", Allowed);
    Options.store(Opts, "ParamAnnot", ParamAnn);
    Options.store(Opts, "TypeAnnot", TypeAnn);
}

void AtomicOnlyCheck::registerMatchers(MatchFinder *Finder) {
    Finder->addMatcher(translationUnitDecl().bind("tu"), this);
}

void AtomicOnlyCheck::check(const MatchFinder::MatchResult &Result) {
    const auto *TU = Result.Nodes.getNodeAs<TranslationUnitDecl>("tu");
    if(!TU) return;

    AtomicOnlyVisitor V(*Result.Context, Result.Context->getDiagnostics(), Allowed, ParamAnn, TypeAnn);
    V.TraverseDecl(const_cast<TranslationUnitDecl *>(TU));
}

} // namespace elysium
