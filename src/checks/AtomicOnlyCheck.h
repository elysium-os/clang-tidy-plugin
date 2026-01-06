#pragma once

#include "clang-tidy/ClangTidyCheck.h"

#include <string>

namespace elysium {

class AtomicOnlyCheck : public clang::tidy::ClangTidyCheck {
  public:
    AtomicOnlyCheck(llvm::StringRef Name, clang::tidy::ClangTidyContext *Ctx);

    void storeOptions(clang::tidy::ClangTidyOptions::OptionMap &Opts) override;
    void registerMatchers(clang::ast_matchers::MatchFinder *Finder) override;
    void check(const clang::ast_matchers::MatchFinder::MatchResult &Result) override;

  private:
    std::string Allowed;
    std::string ParamAnn;
    std::string TypeAnn;
};

} // namespace elysium
