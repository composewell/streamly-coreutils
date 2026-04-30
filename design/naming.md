## Module Naming

## Problem

Multiple Haskell modules (`touch`, `mv`, `rm`) expose option setters with the
same names (e.g., `force`, `recursive`), causing name conflicts when
aggregating them into a single module.

## Options considered

1. Import modules separately (qualified)
2. Rename functions to be unique (`mvForce`, `rmForce`, etc.)
3. Use typeclasses to overload names
4. Provide an aggregated module with aliases

## Decision

Use separate modules with qualified imports. Each command can have its own
module e.g. Coreutils.Cp for the cp command. It may be easy to remember the
module names if they are based on the utility name.

## Rationale

This avoids naming conflicts without renaming functions, keeps the API
intuitive and avoids unnecessary complexity.

The downside is too many imports. But users get to choose what they
import them as, that gives the flexibility of choosing the prefix
rather than hardocding it in the function name itself. Imports can be
automatically managed.

Things that do not conflict can still be imported as a single import i.e.
as Coreutils.

## Best Effort

Let users bundle it if they want to. We can try to keep the naming
non-conflicting as much as we can such that if someone wants to bundle them in
a single module and re-export it should be possible with least effort.

To avoid conflicts, the options for Cp could be called "CpOptions" and
similarly for each module. Otherwise, we may not be able to export all
commands from a single module which is desirable. Using a unique type
for each command would help us use it unqualified.

The command runner would be called "cp" so it would be "cp :: CpOptions -> IO
()".

## Example usage

`Touch.touch (Touch.create False . Touch.followLinks False) path`
`Mv.mv (Mv.force True) old new`
`Rm.rm (Rm.withForce Rm.Force . Rm.recursive True) path`
