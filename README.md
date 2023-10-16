# Lambda Calculus Interpreters

Interpreters for several extensions of Lambda Calculus.

- Dynamic scope
- Locally nameless representation
- Bound & Free variables
- Definitions

## Running

This project was built with Haskell 9.8.

```shell
stack build
```

To install in the PATH:

```shell
stack install
```

To run without installing:

```shell
stack run <interpreter>
```

### JS

1. Install a version of GHC (`>= 9.8`) with the JS backend enabled. Official instructions: [https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building).
2. Run `cabal build`.
3. The resulting JS code will be exported to: `dist-newstyle/build/javascript-ghcjs/ghc-9.8.1/lambdas-0.1.0.0/x/browser/build/browser/browser.jsexe/`.

**Notes:**
- This process is highly unstable due to the limited support of the JS backend. You may have to run `cabal get <package>` to download a package and adjust its dependencies, so they do not collide.
- Due to lack of support for `Text` by the GHC JS backend, at the moment we're limited to using the inefficient `String` datastructure.

## Interpreters

Lambda Calculus:

- [x] Pure: `untyped`
- [x] Simply Typed (a la Church): `typed`
  - [x] Naturals.
  - [x] Booleans.
  - [x] Operators.
  - [x] Type checking.
  - [x] Fixpoint recursion.
- [x] System F.
  - [x] Type definitions.
