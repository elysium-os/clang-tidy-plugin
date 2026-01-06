#include "GlobalPrefixCheck.h"

#include "clang-tidy/ClangTidyOptions.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tidy;

namespace elysium {

GlobalPrefixCheck::GlobalPrefixCheck(StringRef Name, ClangTidyContext *Ctx) : ClangTidyCheck(Name, Ctx) {
    Prefix = Options.get("Prefix", "g_");
}

void GlobalPrefixCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "Prefix", Prefix);
}

void GlobalPrefixCheck::registerMatchers(MatchFinder *Finder) {
    Finder->addMatcher(varDecl(isDefinition(), unless(anyOf(isImplicit(), isExpansionInSystemHeader()))).bind("var"), this);
    Finder->addMatcher(namedDecl(unless(anyOf(varDecl(), isImplicit(), isExpansionInSystemHeader()))).bind("named"), this);
}

void GlobalPrefixCheck::check(const MatchFinder::MatchResult &Result) {
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

} // namespace elysium
