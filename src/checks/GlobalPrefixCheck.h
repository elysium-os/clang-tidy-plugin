#pragma once

#include "clang-tidy/ClangTidyCheck.h"

#include <string>

namespace elysium {

class GlobalPrefixCheck : public clang::tidy::ClangTidyCheck {
  public:
    GlobalPrefixCheck(llvm::StringRef Name, clang::tidy::ClangTidyContext *Ctx);

    void storeOptions(clang::tidy::ClangTidyOptions::OptionMap &Opts) override;
    void registerMatchers(clang::ast_matchers::MatchFinder *Finder) override;
    void check(const clang::ast_matchers::MatchFinder::MatchResult &Result) override;

  private:
    std::string Prefix;
};

} // namespace elysium
