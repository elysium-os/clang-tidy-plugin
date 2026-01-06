#pragma once

#include "clang-tidy/ClangTidyCheck.h"
#include "clang/Lex/PPCallbacks.h"

#include <array>
#include <optional>
#include <string>
#include <vector>

namespace elysium {

class HeaderNamespaceCheck : public clang::tidy::ClangTidyCheck {
  public:
    HeaderNamespaceCheck(llvm::StringRef Name, clang::tidy::ClangTidyContext *Ctx);

    void storeOptions(clang::tidy::ClangTidyOptions::OptionMap &Opts) override;
    void registerMatchers(clang::ast_matchers::MatchFinder *Finder) override;
    void registerPPCallbacks(const clang::SourceManager &SM, clang::Preprocessor *PP, clang::Preprocessor *) override;
    void check(const clang::ast_matchers::MatchFinder::MatchResult &Result) override;

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
        clang::FileID File;
    };

    struct MacroCandidate {
        std::string Name;
        HeaderInfo Info;
        clang::SourceLocation Loc;
        bool UndefinedInHeader{ false };
    };

    struct ExtraPrefix {
        std::string Raw;
        std::string Lower;
        std::string Upper;
    };

    class NamespacePPCallbacks : public clang::PPCallbacks {
      public:
        NamespacePPCallbacks(HeaderNamespaceCheck &Check, const clang::SourceManager &SM);

        void MacroDefined(const clang::Token &MacroNameTok, const clang::MacroDirective *MD) override;
        void MacroUndefined(const clang::Token &MacroNameTok, const clang::MacroDefinition &, const clang::MacroDirective *Undef) override;
        void EndOfMainFile() override;

      private:
        HeaderNamespaceCheck &TheCheck;
        const clang::SourceManager &SM;
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

    static size_t kindIndex(NameKind Kind);

    void parseSuffixes();
    void parseSpecialPrefixes();
    void parseExtraPrefixes();
    void loadCaseOptions(clang::tidy::ClangTidyContext *Ctx);
    void noteMacroDefinition(const clang::IdentifierInfo *II, clang::SourceLocation Loc, const clang::SourceManager &SM);
    void noteMacroUndefined(const clang::IdentifierInfo *II, clang::SourceLocation Loc, const clang::SourceManager &SM);
    void flushMacroDefinitions();

    static std::string normalizePathFragment(llvm::StringRef Path);
    static std::string normalizeFullPath(llvm::StringRef Path);
    NameCase determineCasePreference(NameKind Kind, clang::tidy::ClangTidyContext *Ctx) const;
    static std::optional<NameCase> caseFromReadabilityValue(llvm::StringRef Value);
    static const char *readabilityCaseKey(NameKind Kind);
    const SpecialPrefixRule *determinePrefixRule(llvm::StringRef FilePath) const;
    static bool pathContainsDirectory(llvm::StringRef Path, llvm::StringRef Fragment);
    bool allowGlobalPrefixedVariable(llvm::StringRef Name, llvm::StringRef Stem, llvm::StringRef Expected, llvm::StringRef ExpectedPlural) const;
    bool isHeaderFile(llvm::StringRef Path) const;
    std::optional<NameKind> classifyDecl(const clang::NamedDecl *ND) const;
    bool isGlobalRecord(const clang::RecordDecl *RD) const;
    bool isGlobalFunction(const clang::FunctionDecl *FD) const;
    bool isGloballyExposed(const clang::NamedDecl *ND, NameKind Kind) const;
    bool gatherHeaderInfo(clang::SourceLocation Loc, const clang::SourceManager &SM, HeaderInfo &Info) const;
    void enforcePrefix(llvm::StringRef DeclName, NameKind Kind, clang::SourceLocation Loc, const HeaderInfo &Info);
    static const char *kindDisplayName(NameKind Kind);
};

} // namespace elysium
