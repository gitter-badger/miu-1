##########################
Miu Language Informal Spec
##########################

:author: Varun Gandhi <theindigamer15@gmail.com>

.. contents::

.. section-numbering::

************
Introduction
************

Note: This document heavily borrows from the nicely written
`F# language spec <https://fsharp.org/specs/language-spec/>`_.

Suggested emojis: U+1f326 🌦 or U+1f36d 🍭 (once/if we have linear types!).
Suggested colors: Primary is #ffb9bd (light pink). See `mycolor.space <https://mycolor.space/>`_ or a similar site for full palettes!

Principles
==========

.. csv-table:: Symbols and mental associations
   :header: Symbol, Association(s), Exception(s)
   :widths: 3, 15, 8

   ``.``, "composition (``.>``, ``<.:``), record/module access, bits (``.&.``)", ""
   ``:``, "type (``:``), composition with 2 args (``:.>``), package access", "special operators (``:=``, ``:-``)"
   ``..``, "Too lazy to write it out
          import (``(..)``, ``(.. - x)``),
          wildcards (``Just ..``), ignored hole (``PartialSignatures``),
          enumeration (``1 .. 5``, ``1 ..= 5``)", ""
   ``|``, "No unifying theme
            or (``|``, ``||``),
            application (``|>``, ``>|>``),
            such that (comprehension/refinement)", ""
   "``,``", "sequence", ""
   ``*``, "applicative (``>*>``)", ""
   ``;``, "monadic (``>;>``)", ""
   ``?``, "Holes (``?1``, ``?a``)", ""
   ``&``, "and (``&``, ``&&``), borrow", ""
   ``!``, "index (``!!``, ``!?``)", ""
   ``->``, "function arrow, then (if/match)", ""
   ``->?``, "pattern", ""
   ``#``, "primitive", ""
   ``@``, "optics?, type applications? memory address?", ""
   ``^``, "", ""

****************
Lexical analysis
****************

Miu files must be valid UTF-8 and not contain unescaped NULL (U+0000) characters.

Notation
========

Whitespace
==========

Whitespace consists of spaces, tabs and newlines::

  regex whitespace = (' '|'\t')+
  regex newline = '\n' | '\r' '\n'
  token whitespace-or-newline = whitespace | newline

Comments
========

#. Single line comments begin with ``--``.
#. Single line doc comments begin with ``--|`` or ``--^``.
#. Single line pragmas begin with ``--#``.

Naturally, single line comments extend to the end of the line.

This can be summarized as::

  token end-of-line-comment = "--"
  token end-of-line-doc-comment = "--|" | "--^"
  token end-of-line-pragma = "--#"

Block comments are not planned (because they make parsing in parallel hard)
but may be added in the future (perhaps with a sequential parser?). If they're
added, the syntax would be similar to Haskell::

  token block-comment-start = "{-"
  token block-doc-comment-start = "{-|"
  token block-pragma-start = "{-#"
  token block-comment-end = "-}"

Shebang
-------

A shebang ``#!`` is allowed at the very beginning of the file following the Unix convention.
For example, the following should work if the file is set as an executable::

  #!/usr/bin/env miu-run

Conditional Compilation
=======================

[TODO: Have a look at pros and cons of Rust's behaviour.]

::

  #ifdef HAVE_TIME
  [TODO: This section.]
  #endif

Identifiers and Holes
=====================

Identifiers
-----------

[TODO: Reserve something for extra uppercasing. Also check if Chinese characters
are allowed here.]

[NOTE: Recently, I've been thinking that we should scrap all this complexity
and have flexible identifiers in the style of Racket. Identifiers which consist
of symbols only may be used as infix operators, with possibly some special casing
for 'o'.]

Legal identifiers have the following specification::

  regex digit-char = '\Nd'
  regex letter-char = '\Lu' | '\Ll' | '\Lt' | '\Lm' | '\Lo'
  regex connecting-char = '\Pc'
  regex combining-char = '\Mn' | '\Mc'
  regex formatting-char = '\Cf'
  regex ident-start-char = letter-char
  regex ident-mid-char =
    | letter-char
    | digit-char
    | connecting-char
    | combining-char
    | formatting-char
    | ' | _
  regex ident-end-char = #
  token ident = ident-start-char ident-mid-char* ident-end-char?
  token open-variant-ident =
    '^' ('\Lu' | '\Lt' | '\Lo') ident-mid-char* ident-end-char?

Holes
-----

There are two kinds of holes:

#. Informative holes - These allow the user to tell the compiler "hey, I don't
   know what should be here, can you give me some suggestions?". Informative
   holes can be named/numbered.
#. Ignored holes - These allow the user to tell the compiler "hey, I know
   there is something here, I don't particularly care about it." They can serve
   as documentation while refactoring without making type signatures very large.

Holes are supported to allow for a better interactive experience::

  regex ident-hole = ?
  regex hole-name-char = letter-char | digit-char
  token hole = ? hole-name-char+
  token pattern-hole = ? hole-name-char+
  token or-pattern-hole = ?|
  token ignored-hole = ".."
  -- NOTE: ignored-hole is not lexed separately; the ".." symbol subsumes it.

Examples::

  let foo = Just 10 : .. Int -- analogous to 'Just @Int 10' in Haskell
  let bar : ?b = f x  -- compiler will suggest the type to fill for ?b
  let baz : ?1 = f2 y
  let qux : ?1 = f3 z -- compiler will suggest an option with the constraint that
                      -- the two ?1's match; the "rewrite action" will include a
                      -- renaming for all ?1 holes

Keywords
========

The following phrases act as keywords in all contexts apart from inside string
literals::

  token ident-keyword =
    rec
    let in as where
    type mod module namespace implicit
    deriving via pattern
    forall exists
    do if else match with
    use open import operator visible
    extern foreign
    volatile atomic

  token contextual-ident-keyword = alias family map default

  token reserved-ident-keyword =
    then
    cotype
    data codata
    constructor
    class instance
    functor comptime tailcall
    throw catch except
    mut mutable
    pat pattern rule
    lemma proof

  token backslash-op = "\\"

  token symbolic-keyword =
    | & \ . : .. ; = ..< ..= ? ?? ! ~
    -> <- -o => <= -!>
    ( ) $(             (| |)
    [ ] $[ [> [< >] <] [| |]
    { } ${ {> {< >} <} {| |}

  token contextual-symbolic-keyword = "=="

  token reserved-symbolic-keyword = `


We need a lot of bracket forms :(

1. Types - Rows (?), Records (``{ }``?), Variants (?), Tuples (?)
           effects (prefix ``[ ]``?), units of measure (postfix ``[ ]``),
           implicit arguments (?),
           refinements (``{| |}``?)
2. Terms - Rows (?), Records ``{ }``, Variants (?), Tuples (?),
           suspensions (``{ }``?, ``~( )``?), units of measure (?),
           sequences (?), quasiquotes (?), macros (?)

Operators
=========

[TODO: What characters will be allowed for Unicode operators.]

Operators are, erm, slightly complicated. The core ideas are as follows:

#. A small set of operators are allowed as single letter operators.
#. The set is expanded to a "common set" (which is used in most places)
   for operators with 2 symbols.
#. Operators beginning with a : are considered constructors except when
   immediately followed by '-', '=' or '.'.
#. Operators with 3 or more symbols additionally allow a large set of characters
   to be enclosed between symbols from the common set,
   including the ASCII 'o' as a stand-in for U+25cb '○'.

The rules are summarized below::

  regex op-okay-sym = + - * / ^ % > <
  regex op-nice-sym = ! & '|' '=' ? @ '.'
  regex op-great-sym = : # $ ;
  regex op-common-sym = op-okay-sym | op-nice-sym
  regex op-any-sym = op-common-sym | op-great-sym

  token unary-op = &mut | & | * | @
  token maybe-unary-op = -

  regex short-binary-op = op-okay-sym
  regex medium-binary-op = op-common-sym (op-great-sym | op-common-sym) \ ".."
  regex long-binary-op =
    | medium-binary-op op-any-sym+
    | dsl-op op-common-sym+
    | op-common-sym (op-any-sym | 'o')+ op-common-sym

  token binary-op = short-binary-op | medium-binary-op | long-binary-op
  token symbolic-constr = ':' (op-common-sym \ dsl-op-second-sym) op-common-sym*

  regex dsl-op-second-sym = '-' '=' '.'
  regex dsl-op = :- := :.
  token symbolic-dsl-op = dsl-op

Modules
=======

Module names are like constructors::

  regex pkg-name-start-char = letter-char | digit-char
  regex pkg-name-end-char =
    | letter-char
    | digit-char
    | connecting-char
    | combining-char
    | formatting-char
    | - | _
  regex pkg-name = package-name-start-char package-name-end-char*
  token mod-import-name = (pkg-name :)? mod-name ('.' mod-name)*

Strings and characters
======================

[TODO: Look at Unicode's own suggested syntax.]

String literals can be specified as follows::

  regex char-escape-char = '\' [\'ntbrafv]
  regex char-simple-char =
    (any char except '\\' '\'' '\n' '\t' '\b' '\r' '\a' '\f' '\v')
  regex unicode-char = '\' 'u' hexdigit{1-6}

  regex char-char = char-simple-char | char-escape-char | unicode-char

  regex string-escape-char = '\' [\"ntbrafv]
  regex string-simple-char
    = (any char except '\\' '"' '\n' '\t' '\b' '\r' '\a' '\f' '\v')
  regex string-char =
    | string-simple-char
    | string-escape-char
    | unicode-char
    | newline

  -- TODO: Consider if spaces should be allowed after '\' to avoid a
  -- surprising lexer error when user accidentally leaves trailing whitespace.
  regex string-elem = string-char | '\' whitespace* newline whitespace*

  token char = ' char-char '
  token string = " string-elem* "

  -- The double-quote is interpreted as a single ", like C#/F#
  regex raw-string-char = (any char but '"') | ""
  regex raw-string-elem = " raw-string-char* "

  token raw-string = r raw-string-elem

  token triple-quoted-string = """ (any char)* """

A good discussion on use-cases for raw strings literals
`Rust #9411 <https://github.com/rust-lang/rust/issues/9411#issuecomment-24894071>`_.

Numbers
=======

Numbers can be written in several ways::

  regex decdigit = [0-9]
  regex hexdigit = digit | [A-F] | [a-f]
  regex octdigit = [0-7]
  regex bindigit = [0-1]

  regex dec-nat = 0 (_ | 0)* | [1-9] (_ | decdigit)*
  regex hex-nat = 0 x (_ | hexdigit)+
  regex oct-nat = 0 o (_ | octdigit)+
  regex bin-nat = 0 b (_ | bindigit)+
  regex dec-exp = (e | E) (+ | -)? (_ | decdigit)+
  regex bin-exp = (p | P) (+ | -)? (_ | decdigit)+

  regex sign = (+ | -)?
  regex dec-float = sign dec-nat . dec-nat? dec-exp?
  regex hex-float = sign hex-nat . hexdigit* bin-exp?

  token nat = dec-nat | hex-nat | oct-nat | bin-nat
  token int = sign nat
  token float = dec-float | hex-float

Line directives
===============

Useful for source code generation to trace back errors.

[TODO: What would a good approach be here?]

Hidden tokens
=============

*******
Grammar
*******

Elements
========

[TODO: Think about pattern guard syntax. It shouldn't make parsing hard.]

At the core of ``case`` and ``if`` statements are ``guards`` (borrowing
terminology from Haskell)::

  guard = expr | pattern

``if`` expressions are multi-way by default::

  if  a | b -> c
      (y ->? Just x) -> q x
      else -> z

``case`` expressions are very similar to ``if`` but have a "head" too::

  case x of
    y & let (w ->? Just z) -> q z
    ..  -> p

[NOTE: The flexibility seems nice to have -- however, I need to find more papers
on efficiently compiling these, especially in more general cases. From an
implementation perspective, it would probably be better to implement and
optimize the simple cases first.]

Operators are allowed as type variables. This can be handy when working with
profunctors and similar higher-kinded type constructors. For example::

  type Lens s t a b =
    forall (~~>). Strong (~~>) => (a ~~> b) -> ((a, c) ~~> (b, c))

is arguably clearer than
::

  type Lens s t a b = forall p. Strong p => p a b -> p (a, c) (b, c)

Syntactic sugar
===============

I should try out ideas from Justin Pombrio's papers on resugaring before
adding a bunch of sugar.

``do`` blocks
-------------

*************
Scoping rules
*************

***********
Expressions
***********

Definition expressions
======================

*******
Pragmas
*******

Rewrite
=======

General rewrite rules like Haskell. It is the user's responsibility to make
sure that the LHS and the RHS have the same semantics.

*******************************
Type definitions and signatures
*******************************

Units of Measure
================

We support units of measure like F#. They act like normal types except:

#. They have algebraic rules of equivalence.
#. They have special syntax.
#. They allow more general identifiers.

Here are some examples::

  type m : Measure
  type s : Measure
  type sqm : Measure = m ^ 2

  let triangleArea : Float64 [m] -> Float64 [m] -> Float64 [sqm]
  let triangleArea base height = 0.5 * base * height

  let distanceTravelled : Float64 [m/s] -> Float64 [s] -> Float64 [m]
  let distanceTravelled speed time = speed * time

Units are inferred generically only upon annotation::

  let square1 (x : Float64 ['u]) = x * x
  -- square1 : Float64 ['u] -> Float64 ['u ^ 2]

  let square2 x = x * x
  -- square2 : Multiply a a => a -> a

Unit brackets bind more tightly than application::

  type XCoords = Array U32[m]
  -- type XCoords = Array (U32 [m])

[TODO: Think about ease of unit conversions. Of course, there shouldn't be any
implicit conversions/subtyping. Perhaps using functors + type generation (via
metaprogramming) can alleviate the burden?]

Construction
============

* Atomic measures: These types have no constructors
* Products: Juxtaposition or using a * sign.
* Quotients:
* Integer powers:
* Dimensionless values: written as 1.
* Type variables: such as ``'u``, ``'v`` and so on. These are distinct from
  usual type variables (such as ``m``) in order to prevent confusion.

Annotations
===========

Just like arbitrary expressions can be annotated with plain types, they can
be annotated with units of measure too::

  let ballSpeed = 10 : Int [m/s]
  let zero = 0.0 : [..]
  -- zero : Floating a => a ['u]

***********
Indentation
***********

The default light syntax is indentation-sensitive, similar to Python, Haskell or F#.
This may be mixed with heavy, C-like syntax (possibly with some restrictions).
[TODO: What restrictions?]

Examples
========

``in`` keyword::

  Light syntax      Heavy syntax

  let foo =         let foo =
    let bar = 10      let bar = 10 in
    bar + bar         bar + bar

``do`` blocks::

  Light syntax                         Heavy syntax

  let printHi = do                     let printHi = do {
    let name <- getString                let name <- getString;
        msg = "Hi "                      let msg = "Hi " in
    print (msg ++ name ++ "!")           print (msg ++ name ++ "!");
                                       }

pattern matching::

  Light syntax      Heavy syntax

  case foo of       case foo of {
    1 | 2 -> x        1 | 2 -> x,
    _ -> y            _ -> y,
                    }

records (tentative)::

  Light syntax     Heavy syntax

  type X = {       type X = {
    a : U32          a : U32,
    b : U32          b : U32,
  }                }

  let x : X = {    let x : X = {
    a = 10           a = 10,
    b = 20           b = 20,
  }                }

Faux tokens
===========

We use some fake tokens to avoid handling indentation directly in the parser::

  token $in
  token $begin  -- corresponds to {
  token $end    -- corresponds to }
  token $term   -- corresponds to ;
  token $next   -- corresponds to ,

Grammar rules with faux tokens
==============================

***********
Type system
***********

[NOTE: This section serves as a scratch-pad for now.]

These should be easy to use and on by default:

* OCaml-based

  + polymorphic variants
  + row polymorphic records

    - duplicate fields allowed? - see Koka, Purescript
    - duplicate fields disallowed? - see Ur/Web
    - custom/multiple row theories? - see Morris and McKinna's
      "Abstracting Extensible Data Types"

  + modules and applicative ML functors
    I need to study mixin modules better though, particularly MixML & Backpack.

* Haskell-based
  + GADTs
  + higher-kinded types
  + rank-N types (possibly rank-N types)
  + existential types
  + liquid/refinement types

* some form of linear/affine types

* effect system
  What style though? Eff, Koka/Purescript, Frank/Unison?
  Right now, I'm leaning towards Frank-style effects because of lack of
  explicit effect variables in most cases, which just obscure what's going
  on underneath. That said, I don't understand the paper well -- trying out
  a naive implementation under miuhi (using Haskell for easier
  prototyping).

* (almost?) first class modules

Needs more thought/research:

* coercion
* functional dependencies (desugar to type families?)
* type families (with limited partial application?)
  The paper "Higher-order type-level programming in Haskell" might be
  helpful here. There certainly seems to be some overlap with modules, so are
  these really useful as a separate feature? What about abstraction?
  Is the "global-ness" of type family definitions anti-modular?
* levity polymorphism instead of sub-kinding?
* evaluation-order polymorphism?
* generative functors

Most likely not going to include:

* full dependent types - first-class modules are already very good

****************
Implicit modules
****************

We allow for local defaulting for implicits::

  -- (>) : Ord a => a -> a -> a

  let speedCmps = do
    let default BytecodeSpeedOrd : Ord Bytecode
    assert (fastCode > slowCode)

  let sizeCmp = do
    let default BytecodeSizeOrd : Ord Bytecode
    assert (fastCode < slowCode)

**********
Namespaces
**********

[NOTE: This section serves as a scratch-pad for now.]

OCaml doesn't have namespaces. Over the years, there have been a bunch of
proposals.

1. `Namespaces for OCaml: a proposal <http://gallium.inria.fr/~scherer/namespaces/spec.pdf>`_
2. `A Proposal for Non-Intrusive Namespaces in OCaml <https://ocaml.org/meetings/ocaml/2014/ocaml2014_8.pdf>`_
3. `lpw25/namespaces <https://github.com/lpw25/namespaces>`_

It isn't entirely clear to me what the tradeoffs there are and what the exact
design is.

Some common sense thoughts -

1. Namespaces should contain only other namespaces or modules. F# allows
   namespaces to contain types but we shouldn't do that.
2. Potentially be extensible across packages? This complicates name lookup, so
   I'm not sure if this is a good idea.

Ideally, we don't want to introduce different kinds of separators::

    -- Looks kinda' ugly
    mycompany.package::MyModule.Type
    -- Looks a bit odd?
    mycompany.

Should package names be allowed to be used in the path? If we do that,
they would prevent us from swapping out implementations while preserving
the interface if someone does use the package name explicitly.

*******
Prelude
*******

[TODO: This chapter should only give a short high level overview of the design
of the Prelude and what things are required from alternate preludes. It
shouldn't have anything that would fit better in the library documentation.]

::

  type Monad (m : Type -> Type) = {
     include (Applicative m ⊔ Bind m)
     val leftIdentity : (a : Type) -> (x : a) -> Lemma { bind (pure x) f == f x }
     val rightIdentity : (a : Type) -> (x : a) -> Lemma { bind x pure == x }
  };
