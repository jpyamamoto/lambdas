# Lambda Calculus Interpreters

Interpreters for several extensions of Lambda Calculus.

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
- [x] Simply Typed (a la Church): `typed`
  - [x] Naturals.
  - [x] Booleans.
  - [x] Operators.
  - [x] Type checking.
  - [ ] Fixpoint recursion.
- [ ] System F.
  - [ ] Type definitions.
- [ ] Algebraic Datatypes and Records.

