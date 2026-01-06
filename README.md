# Clang Tidy Plugin

This clang tidy plugin are a random assortment of checks for Elysium.  
Currently this includes the following checks:

- `elysium-atomic-only` Check that ensures atomics are only used by `__atomic_*` builtins, whitelisted functions, or whitelisted parameters.
- `elysium-global-prefix` Check that enforces that every global variable definition uses the configured prefix and that only globals may do so.
- `elysium-header-namespace` Check that enforces globally visible declarations inside headers are prefixed with the header's filename, providing a C-style namespace with configurable casing per symbol kind.

Check options:

- `elysium-atomic-only.AllowedFns` Regex for whitelisted paths that are allowed to take atomic pointers.
- `elysium-atomic-only.ParamAnnot` Annotation that allows an atomic to be derefed as a parameter.
- `elysium-atomic-only.TypeAnnot` Annotation that marks a variable, member, etc as an atomic.
- `elysium-global-prefix.Prefix` Prefix that must be used by globals and may not be used by any other symbol (default `g_`).
- `elysium-header-namespace.HeaderSuffixes` Semicolon- or comma-separated list of header extensions that should be checked (default `.h;.hh;.hpp`).
- `elysium-header-namespace.SpecialDirectoryPrefixes` Optional semicolon- or comma-separated list describing directory-specific prefix overrides. Each entry is `relative/path:prefix_` and, if the header being checked lives within that path (longest match wins), the provided prefix is prepended before the header stem. The suffix `_` is not implied; include whatever separator you prefer. When casing rules demand uppercase (e.g. macros), the directory prefix is uppercased automatically to match.
- `elysium-header-namespace.SpecialGlobalPrefixes` Optional semicolon- or comma-separated list of extra prefixes that may appear before any header-derived prefix. Each entry is treated literally (aside from automatic lower/upper variants to follow casing rules) and may itself include underscores.

The header namespace check automatically derives its `lower`/`upper` casing preferences from the corresponding `readability-identifier-naming.*Case` options (falling back to lowercase when they are unset), so you only need to configure casing once. Parameters, members, and other local declarations are ignored by this check.

Macros that are defined and undefined within the same header are exempt from the namespace rule; they may use any name because the check can prove they do not leak beyond the header.
