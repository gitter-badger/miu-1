# Miu's Values

Bryan Cantrill (author of dtrace) has this neat talk about the "values" of
software platforms, such as programming languages.

https://youtu.be/2wZ1pCpJUIM?t=261

He talks about how those values guide design, and affect suitability for
different problem domains.

(I recommend watching the talk if you haven't already seen it.)

The values Bryan Cantrill lists (he admits the list is a bit random and
certainly not exhaustive) are:

* Approachability
* Availability
* Compatibility
* Composability
* Debuggability
* Expressiveness
* Extensibility
* Interoperability
* Integrity
* Maintainability
* Measurability
* Operability (not sure what this means...)
* Performance
* Portability
* Resiliency
* Rigor (not 100% sure what this means in this context... does insisting on a
  sound type system mean that you value rigor?)
* Robustness
* Safety
* Security
* Simplicity
* Stability
* Thoroughness
* Transparency
* Velocity

I think, for me, the most important attribute thing is *clarity*. What does
that mean?

## Clarity

### Clear syntax

The syntax of values, computations and types makes it clear what's going on.
Even if certain details are elided by the compiler, it should *always* be
possible to spell those explicitly, for pedagogy and understanding. Some
specific points:

* It must be possible to add type annotations to pin/check inferred types to
  full accuracy. This entails the absence of "unnameable" types.
* It must be possible to use hand-written interface files like in ML for
  separate compilation. This includes potentially writing down descriptions
  of unboxed memory representations.
* Inlining aliases (types or values) should not change the (static or dynamic)
  semantics of the program.
* "Clear" types - Types should be honest about the values inhabiting them.
  This means that we track effects, including non-termination, and separate
  values from computations (c.f. Lindley et al.'s "Do Be Do Be Do").

## Expressiveness

Initially, the main reason I started out to create a new language because I
felt I was running into walls in different languages. In Rust, it is with
expressing more complicated ownership structures. In Haskell, it is in
working with unboxed types and mutability. The key "motto" I had in my mind was:

    Let me say what I want to say without getting in the way.

In Haskell, it is not possible to say "hey, please monomorphize this data type for
me in every situation, I'm willing to give up X, Y, Z" without writing a
bunch of gross code using type families for specialization.

In both Rust and Haskell, the canonicity of type class instances occasionally
makes things painful (the "without getting in the way" part doesn't hold).

Working with type-level numbers is painful because of the manual proof burden.
Why can't I put my trust in an SMT solver, instead of proving all the little
identities and implementing so many functions at the type level, greying a bunch
of hair in the process?

## Composability

## Closure property

"Closure" here has two distinct but related notions -

1. Gradual transitions/no holes - It should be possible to transition between
   different points in the design space in "small" steps. Put another way,
   if there is a high cost/high benefit way of doing something and a
   low cost/low benefit way of doing something, adding a medium cost/medium
   benefit feature to ease a transition between the two extremes is not bad.

   For example, anonymous records present an intermediate point between
   one-off and ad-hoc tuples. Similarly keyword arguments are an intermediate
   point between positional arguments and one-off data types. Even in the
   presence of anonymous records, keyword arguments are valuable in a context
   where functions are curried by default.

2. No sharp edges/roundedness - This is best explained with an example.
   Consider alias/name shortening as one dimension and language constructs
   on the other dimension. Say it is initially possible to write type aliases
   and value aliases. Next, say we add modules. The "roundedness" idea
   implies that the missing element in the "matrix" should be filled -
   we should have modules aliases.
