# Clang Tidy Plugin

This clang tidy plugin are a random assortment of checks for Elysium.  
Currently this includes the following checks:
- `elysium-atomic-only` Check that ensures atomics are only used by `__atomic_*` builtins, whitelisted functions, or whitelisted parameters.

Check options:
- `elysium-atomic-only.AllowedFns` Regex for whitelisted paths that are allowed to take atomic pointers.
- `elysium-atomic-only.ParamAnnot` Annotation that allows an atomic to be derefed as a parameter.
- `elysium-atomic-only.TypeAnnot` Annotation that marks a variable, member, etc as an atomic.
