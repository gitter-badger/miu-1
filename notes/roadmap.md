# Roadmap?

* First, I want to have a very fast compiler
  - Elm-like subset
    - Rank-1 polymorphism
    - Lazy values
    - Anonymous records with no extension/subtyping
    - Minimal global inference, bidirectional type-checking
  - Compile to x86_64 at 100k lines/sec or faster with non-trivial
    instruction selection and register allocation.
  - No incrementality
  - We keep the tree-sitter parser for now, unless it proves to
    be taking a significant chunk of time.

Cut down even more?
  - No polymorphism
  - No lazy values
  - No type inference (including lambdas), only type-checking
  - How to handle IO though? Use impurity for now?

Some problems though:

- Incrementality cannot just be bolted on.
- IDEs have pretty different requirements.
- Pipelining cannot be just bolted on later - Kentucky Mule uses pipelining
  in the middle of the dev process.
- E.g. DMD and the Nim compiler are fast but not incremental.

Should we have two compilers, or at least two front-ends? That seems like a lot
of work for the beginning, where people can
