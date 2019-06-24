# Compiler users

1. Person
   - Needs: Too many
2. Autofixer (refactoring engine? migrator?)
   - Needs: potentially everything
     (from concrete syntax tree to type-checked output)
3. Formatter
   - Needs: concrete syntax tree and operator fixities.
4. Linter (e.g. HLint)
   - Needs: name-resolved syntax tree and pragmas (e.g. "disable lint 57").
5. Documentation generator
   - Needs: comments and name-resolved syntax tree (for cross-linking).
6. Search/Indexer (use Kythe?)
   - Needs: comments + typed tree
7. Build system
   - ???
8. Package manager
   - ???
9. IDEs
   - [Works on 1 project at a time](https://www.reddit.com/r/rust/comments/a6ph8p/rust_analyzer_in_2018_and_2019/ebxjf70),
     unlike the compiler which will work on 1 package at a time.
   - Needs both concrete syntax, as well as type-checked output.
   - See the Rust RFC for [libsyntax2](https://github.com/matklad/rfcs/blob/libsyntax2.0/text/0000-libsyntax2.0.md#ide-support>`)
   - Can the compiler expose APIs that work with an IDEs implementation of a
     virtual file system, avoiding duplication of work?
     https://www.jetbrains.org/intellij/sdk/docs/basics/virtual_file_system.html
10. External syntax highlighter (not super important)
    - Needs a concrete syntax tree
11. Type debugger
    - ???
12. Macro debugger
    - ???
13. Run-time debugger
    - ???
14. Profiler
    - ???

# Passes

Where does macro expansion go? Tentative compiler passes

```
           Text
            ↓  Lexing
       Token stream (explicit space)
            ↓  Indent-check
       Token stream (implicit space)
            ↓  Parsing
     Concrete Syntax Tree
            ↓  Name/fixity resolution
       Named Syntax Tree
            ↓  Macro expansion?
       Named Syntax Tree
            ↓  ASTGen
     Abstract Syntax Tree
            ↓  Type-check
    Decorated Syntax Tree
            ↓  IRGen
            IR
```

# Compilation speed

## Fast cold compile times.

Sorbet (for Ruby in C++) type-checks at 100,000 lines/sec
[May 29, 2019 | [reddit](https://www.reddit.com/r/haskell/comments/bu7big/haskell_weekly_podcast_episode_11_profiling/ep9phqv?utm_source=share&utm_medium=web2x)].

The DMD compiler compiles itself in < 7s for roughly 300kloc
[Nov 18, 2018 | [blog](https://blog.thecybershadow.net/2018/11/18/d-compilation-is-too-slow-and-i-am-forking-the-compiler/)].

The Jai compiler compiles (including code gen) at ~100,000 lines/sec
[Dec 22, 2018 | [YouTube](https://youtu.be/-9c095aXc-s?t=9980)].

I'm not sure how realistic it is to have similar compilation speeds
(especially with more sophisticated type system features) but we should at
least aim for numbers within a factor of 2.

(TODO: I should benchmark the bootstrapping time on my machine for the OCaml
compiler, DMD, and the Nim compiler.)

### Aside: what will a "typical" Miu module look like size-wise?

This might be helpful in generating fake codebases to test compiler speed on
realistic dependency graphs.

Typical number of top-level declarations in a Haskell file:

```
rg -c "::|data|type|newtype|instance" | \
awk -F ":" '$1~/.hs/{sum += 1; total += $2} \
  END{print "total:" total, "files:" sum, "avg:" (total/sum)}'
```

The numbers are ~71 for ``containers/Data`` and ~85 for ``ghc/compiler``.

Similarly for imports statements, the numbers are: ~10 for ``containers/Data``
(this seems awfully low) and ~21 for ``ghc/compiler``.

TODO: Make a table here and use other repos like Yesod/Opaleye.

## Separate compilation

Compile times for changes are `O(1)`. We utilize user-written (or the compiler
could generate these the first time) interface files, and sacrifice optimizations
(e.g. cross-module inlining) for the sake of compile times.

## Incremental compilation

Compile times are `O(d)`, where `d` is the depth of the module where the change
is being made. This should support early cutoff so we don't show cascading errors.

## Pipelined compilation

### Value-level pipelining

(Top-level-definition pipelining is probably more accurate. Oh well.)

In the graph of intra-module dependencies, it is possible that computing extra
information allows the rest of a pass (e.g. type-checking) to be done in
parallel. At present (June 2019), GHC only has parallelism at the granularity of
a module, meaning that if there is 1 module in the DAG which is taking a lot
of time to compile, it will use only 1 core.

Kentucky mule is a good example of value-level pipelining -

1. [Can Scala have a highly parallel typechecker?](https://medium.com/@gkossakowski/can-scala-have-a-highly-parallel-typechecker-95cd7c146d20)
2. [Kentucky Mule — limits of Scala typechecking speed](https://medium.com/@gkossakowski/kentucky-mule-limits-of-scala-typechecking-speed-6a44bd520a2f)

Also see comments in this [Github issue](https://github.com/gkossakowski/kentuckymule/issues/6).

[Hack](https://www.youtube.com/watch?v=uXuYVUdFY48&t=346s) (parallel
type-checker for Facebook's PHP dialect was an inspiration for Kentucky
mule.

Some key takeaways from the text and the video:

1. Understanding data dependencies is very important. Hack gets away with
   type-checking function bodies in parallel because it doesn't do function
   level inference.
2. For performance, one may have to inline/duplicate stuff. For example,
   Hack inlines declarations of superclasses into subclasses.
3. One needs to carefully think about invariants that are needed. Josh talks
   about an issue where they didn't have the guarantee (for a hash table)
   that if a write returns, then subsequent reads are successful
   (this can happen if multiple threads are trying to write and one
   returns while it is actually the second one that is still writing to
   the table), which caused hard-to-track-down issues.

What does this mean for Miu(ri)?

1. I really do NOT want to skip over having function level inference.
   If you jump into code that is way too complex for you/are having a hard
   time understanding the type-checker, a good way to get things to work
   is to delete function signatures and fill inferred ones.

   However, one thing that we could do (and probably should) is assume that
   most functions are annotated with *correct* signatures. This means that
   dependencies need not be blocked on the definition being type-checked.
   In case there are two type errors, one from the definition and one from
   a caller, then we can try to infer the type of the definition and issue
   a better error message (whether the signature needs to be changed).

   Look at SHErrLoc for details. One big wrinkle is that I probably want to
   go with a bidirectional system whereas SHErrLoc is designed to work with
   a constraint graph generated from a HM-style inference algorithm.

   Another big wrinkle with this plan is that complex signatures will usually
   involve a bunch of implicits and OCaml/Scala do not infer implicit
   parameters...

### Module-level pipelining

In the DAG of module dependencies, emit information needed by callers, so they
can start compilation, before continuing with leftover work. (This doesn't work
directly with macros, see [this section](#incrementality-in-the-presence-of-macros).)

In our case, this kinda' affects parsing in two different ways.

1. User-defined operators with custom associativity and fixities -

   There are a couple of options here. It would be nice to benchmark the
   difference, but it would probably take a substantial amount of effort to
   do so...

   1. GHC-style tree rebalancing - this has the benefit that files can be
      parsed in parallel without dependencies across files. Since the renaming
      stage already depends on the parsing stage, this implementation is
      potentially simpler. This also allows more flexibility to the user of
      being able to define operators anywhere.

   2. One-shot parsing - this has the benefit of not have to perform a large
      number of tree rotations at the cost of introducing dependencies at the
      parsing stage (typical Haskell code has a lot of operators and I'm
      guessing typical Miu code will too). In effect, parsing will split up
      into two phases:

      1. P1: Import + operator definition parsing.
      2. P2: Module body parsing.

      P1 can be done in parallel across all files. Work on P2 can be started
      for a file once P1 has finished for files corresponding to imports. Note
      that this doesn't create a problem even if we allow cylic dependencies
      between files.

2. Macros (hygienic or otherwise) - These are actually even more terrible
   because macro expansion (for untyped macros) can affect the dependency
   graph across modules. See [this section](#incrementality-in-the-presence-of-macros).

### Package-level pipelining

In the DAG of package dependencies, emit information needed by callers, so they
can start compilation, before continuing with leftover work.

Rust recently added this.

### Incrementality in the presence of macros

(For now, let's assume that macros *do not* have access to type machinery,
unlike Hackett. Otherwise, we have an even hairier problem on our hands.)

(Sloppiness note: It is not really relevant whether we're talking about
type definitions or value definitions, so when I say "expression", it should
be assumed that I'm also including type definition slots, pattern positions,
etc. In the end, we will probably scale down the flexibility so that the
resulting system can be implemented in a typed language in a sane way,
otherwise literally every field in the AST might have a macro expansion in it
:-/.)

In the presence of macros, coarse-grained dependency tracking at the level of
modules is clearly insufficient, as we need to redo all macro expansions
on every change to make sure that there is no effect on the generated code. That
is not a very incremental approach. Instead, we can take a more fine grained
approach -

* After/during first compilation, we divide the entire project code into
  "code contexts" (these are like evaluation contexts in that they have "holes"
  in them which will be filled by macro-expanded code) and *user-written macro
  definitions*. (It is unclear how fine-grained code contexts should be, maybe
  they shouldn't be more fine-grained than 1 top-level definition.)

  ```
  module ::= code-context
           | macro-definition
           | module module+

  code-context ::= top-level-definition+
                 | (holey-top-level-definition, hole+)
  ```

  For example, the code context for `let x = 1 + foo!(boop)`, has a body
  `let x = 1 + ▢` with 1 hole (U+25a2), filled by `foo!(boop)`.

* The final expanded code is at phase 1. The "effective expanded code" at phase
  n+1 is inductively defined as the code, which if going through 1 full round of
  macro expansion, creates the effective expanded code at phase n.

  (I think this is basically what Matthew Flatt is saying in his Strange Loop talk
  "Let's build a hygienic macro expander". I need to look at his paper
  [Binding as sets of scopes](http://www.cs.utah.edu/plt/publications/popl16-f.pdf)
  more thoroughly. I should also read more on Racket's
  [syntax model](https://docs.racket-lang.org/reference/syntax-model.html)).

  A macro expands to a sequence of "snippets" and macro definitions. A snippet
  is essentially a code context, but with expression level granularity.

  ```
  snippet ::= code-context | (expression, hole*)
  ```

  For example, if `foo!(boop)` expands to `boop!(blep) * 10`, the body
  consists of `▢ * 10` with 1 hole, filled by `boop!(blep)`.

  Top-level macros expands to full contexts but the idea is similar.

* On perfoming macro expansion, we add edges from each hole to the expanded
  code. We add an entry to the dependency DAG here, the transformation edge
  from hole -> code depends on the macro being expanded. Considering our running
  example. Say the full code is something like:

  ```
  macro boop!(x) = x ^ x ;
  macro foo!(i) = i!(blep) * 10 ;
  let x = 1 + foo!(boop) ;
  let blep = 3
  ```

  The dependency graph built using this looks like

  ```
  3  macro foo! = ...      foo!(boop)
            +-------------> /       \
                           v         \
  2  macro boop! = ...  boop!(blep)   +
           +-------------->|          |
                           v          v
  1 let x = 1 + ▢     blep ^ blep    ▢ * 10  ; let blep = 3
                      /
                     v
  0              x <--- blep
  ```

  (Using phase 0 = type-checking here but typically it is used for run-time.)

  All user-defined items have no parents in the tree. All other nodes have one
  immediate parent, with the corresponding descendancy edge depending on either
  a leaf node (a user-defined macro) or an internal node (a macro-defined macro).

  If we preserve all intermediate nodes, changing the definition for `boop!`
  means that we only need to change the places where `boop!` is being expanded
  (and its descendants), avoiding the need to recompute the expansion of
  `foo!(boop)`. Another thing is that we need to recompute the dependencies
  for `x` but we don't need to do anything for `blep` itself. This means that
  if `blep` was type-checked earlier against a declared type, we don't need
  to type-check it again!

  (It probably makes more sense to treat the definition and macro-call as equal
  "parents" in the build DAG.)
