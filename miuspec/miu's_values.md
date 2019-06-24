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

### Clear types

Types should be honest about the values inhabiting them. This means that we will
track effects, and separate values from computations (c.f. Lindley et al.'s
"Do Be Do Be Do").

Exception: While non-termination does qualify as an "effect" in my book, at
this stage, it isn't clear to me what the costs of tracking non-termination are
in terms of friction, library design and implementation complexity. I do not
want to have "functions of different colors" for compile time evaluation like
Rust and C++.)

## Composability

In some sense, the magic of Haskell is in its composability. So many functions
can be used in contexts that you might not have thought of before.
The key reasons for these are (in no particular order) - laziness
(abstraction over control flow), polymorphism (abstraction over types) and
garbage collection (abstraction over memory management).

One of the reasons laziness is not used all that often in strict languages is
because it is unwieldy. We should explore ways to make it easier to use, such
as Idris's automatic insertion.

### Modularity

Modularity is a key aspect of composability. We should be able to put two
units ("modules") together, pick and choose which parts we want from each,
and use both in the same program.

Specifically, this rules out the use of Haskell-style type classes, or any
similar language features which enforce global requirements.

## Expressiveness

Initially, the main reason I started out to create a new language because I
felt like I was running into walls in different languages. In Rust, it is with
expressing more complicated ownership structures without using reference counting.
In Haskell, it is in working with unboxed types. The key "motto" I had in mind was:

    Let me say what I want to say without getting in the way.

In Haskell, it is not possible to say "hey, please monomorphize this data type for
me in every situation, I'm willing to give up X, Y, Z" without writing a
bunch of gross code using type families for specialization.

In both Rust and Haskell, the canonicity of type class instances occasionally
makes things painful (the "without getting in the way" part doesn't hold). If I
want something like ML modules, I need to go through a painful encoding process
at the type level, or use macros, replacing semantic abstraction with syntactic
abstraction.

## Closure property

Closure here has two distinct but related notions -

1. No "chasms" - It should be possible to transition between
   different points in the design space in "small" steps. Put another way,
   if there is a high cost/high benefit way of doing something and a
   low cost/low benefit way of doing something, adding a medium cost/medium
   benefit feature to ease a transition between the two extremes is not bad.

   For example, anonymous records present an intermediate point between
   ad-hoc tuples and one-off data types. Similarly keyword arguments are an
   intermediate point between positional arguments and one-off data types.
   Even in the presence of anonymous records, keyword arguments are valuable
   if your functions are curried by default.

   Let me expand on the anonymous records for a bit. Often, I've seen people
   argue against such "sugary" features using "Oh, just define a data type!"
   or "I've never used keyword arguments (in other $lang) and I've never
   missed it *shrug*". Such an argument is harmful because -

   * Defining a data type involves more friction - whether that friction is
     too high or reasonable depends on the person *writing* the code, not the
     person suggesting the alternative.

   * More generally, such arguments often focus only on the technical aspects
     (e.g. does this add expressiveness to the language?) and miss out on
     the emotional aspect. But. Empathy is very important.
     If someone's saying, "Oh! That's too painful", saying "No, just do that"
     is effectively saying "I don't care about how you feel".

   That said, I don't mean that syntactic sugar should be added willy-nilly.
   But if some kind of sugar helps bridge such a gap between two extreme cases,
   it should be a plus point in its favor.

2. Roundedness - This is best explained with an example.
   Consider alias/name shortening as one dimension and language constructs
   on the other dimension. Say it is initially possible to write type aliases
   and value aliases. Next, say we add modules. The "roundedness" idea
   implies that the missing element in the "matrix" should be filled -
   we should have modules aliases.

   Of course, it might be possible that some matrix entries cannot be filled
   for technical reasons - in such a situation, the reasoning should be clearly
   documented.
