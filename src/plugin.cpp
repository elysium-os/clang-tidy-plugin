#include "checks/AtomicOnlyCheck.h"
#include "checks/GlobalPrefixCheck.h"
#include "checks/HeaderNamespaceCheck.h"

#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"

using namespace clang::tidy;

namespace elysium {

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

#ifdef CLANG_TIDY_ENABLE_STATIC_ANALYZER
volatile int ElysiumTidyModuleAnchorSource = 0;
#endif
