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

Principles
==========

.. csv-table:: Symbols and mental associations
   :header: Symbol, Association(s), Exception(s)
   :widths: 3, 15, 8

   ``.``
       , "composition (``.>``, ``<.:``), record/module access, bits (``.&.``)"
       , ""
   ``:``
       , "type (``:``), composition with 2 args (``:.>``), package access"
       , "special operators (``:=``, ``:-``)"
   ``..``
       , "Too lazy to write it out
          import (``(..)``, ``(.. - x)``),
          wildcards (``Just ..``), ignored hole (``PartialSignatures``),
          enumeration (``1 .. 5``, ``1 ..= 5``)"
       , ""
   ``|``, "No unifying theme
            or (``|``, ``||``),
            application (``|>``, ``>|>``),
            such that (comprehension/refinement),
            parallel (comprehension/library ops)"
       , ""
   .. the double-quote is needed to prevent the comma from getting parsed as a
      separator :(
   "``,``", "sequence", ""
   ``*``, "applicative (``>*>``), deref", ""
   ``;``, "monadic (``>;>``)", ""
   ``_``, "Holes (``_1``, ``_a``)", ""
   ``&``, "and (``&``, ``&&``), borrow/address", ""
   ``!``, "index (``!!``, ``!?``)", ""
   ``->``, "function arrow, then (if/match)", ""
   ``<-``, "pattern", ""
   ``#``, "primitive", ""
   ``@``, "optics?", ""
   ``^``, "", ""

****************
Lexical analysis
****************

Miu files must be valid UTF-8.

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
  token open-variant-ident = '^' ('\Lu' | '\Lt' | '\Lo') ident-mid-char* ident-end-char?

Holes
-----

There are two kinds of holes:

#. Informative holes - These allow the user to tell the compiler "hey, I don't
   know what should be here, can you give me some suggestions?". Informative
holes can be named/numbered.
#. Abbreviation holes - These allow the user to tell the compiler "hey, I know
   there is something here, I don't particularly care about it." They can serve
   as documentation while refactoring without making type signatures very large.

Holes are supported to allow for a better interactive experience::

  regex ident-hole = _
  regex hole-name-char = letter-char | digit-char
  token hole = _ hole-name-char+
  token pattern-hole = _ hole-name-char+
  token or-pattern-hole = _|
  token abbrev-hole = ".."
  -- NOTE: abbrev-hole is not lexed separately; the ".." symbol subsumes it.

Examples::

  let foo = Just 10 : .. Int -- analagous to 'Just @Int 10' in Haskell
  let bar : _b = f x  -- compiler will suggest the type to fill for _b
  let baz : _1 = f2 y
  let qux : _1 = f3 z -- compiler will suggest an option with the constraint that
                      -- the two _1's match; the "rewrite action" will include a
                      -- renaming for all _1 holes

Keywords
========

The following phrases act as keywords in all contexts apart from inside string
literals::

  token ident-keyword =
    rec
    let in as and where
    type mod implicit deriving
    forall exists
    do if then else match with
    import operator
    foreign volatile
    atomic

  token contextual-ident-keyword = alias family map default

  token reserved-ident-keyword =
    cotype
    data codata
    class instance
    functor comptime tailcall
    throw catch except

  token backslash-op = "\\"

  token symbolic-keyword =
    | & \ . : .. ; = ..= ? ??
    -> <- -o => <=
    ( ) $(
    [ ] $[ [> [< >] <] [| |]
    { } ${ {> {< >} <}
    -[ident]->
    =[ident]=>

  token contextual-symbolic-keyword = "=="

  token reserved-symbolic-keyword = `

Operators
=========

Operators are, erm, slightly complicated. The essential idea is that:

#. A small set of operators are allowed as single letter operators.
#. The set is expanded to a "common set" (which is used in most places)
   for operators with 2 symbols.
#. Operators beginning with a : are considered constructors except when
   immediately followed by '-', '=' or '.'.
#. Operators with 3 symbols additionally allow a large set of characters
   to be enclosed between symbols from the common set,
   including the ASCII 'o' as a stand-in for U+25cb '○'.

The rules are summarized below::

  regex op-okay-sym = + - * / ^ % > < ~
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

[TODO: This is very low priority for now.]

Hidden tokens
=============

*******
Grammar
*******

Elements
========

[TODO: Think about pattern guard syntax. It shouldn't make parsing hard.]

At the core of ``match`` and ``if`` statements are ``guards`` (borrowing
terminology from Haskell)::

  guard = expr | pattern

``if`` expressions are multi-way by default::

  if  a | b -> c
      (Just x <- y) -> q x
      else -> z

``match`` expressions are very similar to ``if`` but have a "head" too::

  match x with
    y & let (Just z <- w) -> q z
    ..  -> p

Operators are allowed as type variables. This can be handy when working with
profunctors and similar higher-kinded type constructors. For example::

  type Lens s t a b = forall (~>). Strong (~>) => (a ~> b) -> ((a, c) ~> (b, c))

is arguably clearer than
::

  type Lens s t a b = forall p. Strong p => p a b -> p (a, c) (b, c)

Syntactic sugar
===============

``do`` blocks
-------------

View patterns
-------------

Comprehensions
--------------

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

General rewrite rules like Haskell.

Impl
====

Rewrite
-------

The function implementation should be treated as a rewrite rule (with argument
expressions directly substituted), instead of first evaluating the arguments
and then calling the function. For example, boolean short-circuit operations
can be implemented in a library using this technique::

  --# Impl [Rewrite]
  (&&) x y = match x with
    True  -> y
    False -> False

[TODO: This is a special case of a more general rewrite rule.]

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

  --# Measure
  type m
  --# Measure
  type s
  --# Measure
  type sqm = m ^ 2
  let triangleArea : F64 [m] -> F64 [m] -> F64 [sqm]
  let triangleArea base height = 0.5 * base * height

  let distanceTravelled : F64 [m/s] -> F64 [s] -> F64 [m]
  let distanceTravelled speed time = speed * time

Units are inferred generically only upon annotation::

  let square1 (x : F64 [..]) = x * x
  -- square1 : F64 ['u] -> F64 ['u] -> F64 ['u ^ 2]

  let square2 x = x * x
  -- square2 : Multiply a => a -> a -> a

Unit brackets bind more tightly than application::

  type XCoords = Array U32[m]
  -- type XCoords = Array (U32 [m])

[TODO: Think about ease of unit conversions.]

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
    putStrLn (msg ++ name ++ "!")        putStrLn (msg ++ name ++ "!");
                                       }

module declarations::

  Light syntax                 Heavy syntax

  mod Foo where                mod Foo {
    type Bar = Int               type Bar = Int;
    let double : Bar -> Bar      let double : Bar -> Bar;
    let double = (* 2)           let double = (* 2);
                               }

pattern matching::

  Light syntax      Heavy syntax

  match foo with    match foo {
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

*******
Prelude
*******

[TODO: This chapter should only give a short high level overview of the design
of the Prelude and what things are required from alternate preludes. It
shouldn't have anything that would fit better in the library documentation.]

Some amount of built-in support for (profunctor) optics?
