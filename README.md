# Lambda Calculus Interpreters

Interpreters for several extensions of Lambda Calculi.

- Dynamic scope
- Locally nameless representation
- Bound & Free variables
- Definitions

## Running

This project was built with Haskell 9.2.

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

## Interpreters

Lambda Calculus:

- [x] Pure: `untyped`
- [ ] Simply Typed (a la Curry): `typed`
  - [x] Naturals
  - [x] Booleans
- [ ] Simply Typed (a la Church).
  - [ ] Typing definitions.
- [ ] Simply Typed with fixpoint recursion.
- [ ] Universal Quantification.
- [ ] Algebraic Datatypes and Records.

