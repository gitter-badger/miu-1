##########################
Miu language informal spec
##########################

Note: This document heavily borrows from the nicely written
`F# language spec <https://fsharp.org/specs/language-spec/>`_.

************
Introduction
************

Suggested emojis: U+1f326 🌦 or U+1f36d 🍭 (once/if we have linear types!).

Notational conventions
======================

****************
Lexical analysis
****************

Miu files must be valid UTF-8.

Whitespace
==========

Whitespace consists of spaces, tabs and newlines::

    regex whitespace = (' '|'\t')+
    regex newline = '\n' | '\r' '\n'
    token whitespace-or-newline = whitespace | newline

Comments
========

#. Block comments are delimited by ``{-`` and ``-}``.
#. Block doc comments are delimited ``{-|`` and ``-}``.
#. Block pragmas are delimited by ``{-#`` and ``-}``.
#. Block comments cannot be nested.
#. Single line comments begin with ``--``.
#. Single line doc comments begin with ``--|`` or ``--^``.
#. Single line pragmas begin with ``--#``.

Naturally, single line comments extend to the end of the line.

This can be summarized as::

  token block-comment-start = "{-"
  token block-doc-comment-start = "{-|"
  token block-pragma-start = "{-#"
  token block-comment-end = "-}"
  token end-of-line-comment = "--"
  token end-of-line-doc-comment = "--|" | "--^"
  token end-of-line-pragma = "--#"

Shebang
-------

A shebang ``#!`` is allowed at the very beginning of the file following the Unix convention.
For example, the following should work if the file is set as an executable::

  #!/usr/bin/env miu-interpret

Conditional Compilation
=======================

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
  regex ident-mid-char = letter-char | digit-char | connecting-char | combining-char | formatting-char | ' | _
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
   there is something here, infer it, and keep your mouth shut." They can serve
   as documentation while refactoring without making type signatures very large.

Holes are supported to allow for a better interactive experience::

  regex ident-hole = _
  regex hole-name-char = letter-char | digit-char
  token hole = _ hole-name-char*
  token pattern-hole = __ hole-name-char*
  token or-pattern-hole = __|
  token abbrev-hole = ".."

Examples::

  let foo = Just 10 : .. Int -- analagous to 'Just @Int 10' in Haskell
  let bar : _  = f x  -- compiler will suggest the type to fill for _
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
    import except
    foreign volatile
    atomic

  token contextual-ident-keyword = family map default

  token reserved-ident-keyword =
    atomic functor comptime tailcall

  token backslash-op = "\\"

  token symbolic-keyword =
    -> <- -o | \ . : .. ; = ? ??
    ( ) $(
    [ ] $[ [> [< >] <] [| |]
    { } ${ {> {< >} <}
    -(ident)->
    ->} -o}

  token contextual-symbolic-keyword = "==" "==>"

  token reserved-symbolic-keyword = `

Operators
=========

Operators are, erm, slightly complicated. The essential idea is that:

#. A small number of operators are allowed as single letter operators.
#. The set is expanded to a "common set" (which is used in most places)
   for operators with 2 symbols.
#. Operators beginning with a : are considered constructors except when
   immediately followed by '-', '=' or '.'.
#. Operators with 3 symbols additionally allow the largest set of characters
   enclosed in the common set, including the ASCII 'o' as a stand-in for
   U+25cb '○'.
::
  regex op-okay-sym = + - * / ^ % > < ~
  regex op-nice-sym = ! & '|' '=' ? @ '.'
  regex op-great-sym = : # $ ;
  regex op-common-sym = op-okay-sym | op-nice-sym
  regex op-any-sym = op-common-sym | op-great-sym

  token unary-op = &mut | & | @
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
  regex pkg-name-end-char = letter-char | digit-char | connecting-char | combining-char | formatting-char | - | _
  regex pkg-name = package-name-start-char package-name-end-char*
  token mod-import-name = pkg-name : mod-name ('.' mod-name)*

Strings and characters
======================

String literals can be specified as follows::

  regex char-escape-char = '\' [\'ntbrafv]
  regex char-simple-char = (any char except '\\' '\'' '\n' '\t' '\b' '\r' '\a' '\f' '\v')
  regex unicode-char = '\' 'u' hexdigit{1-6}

  regex char-char = char-simple-char | char-escape-char | unicode-char

  regex string-escape-char = '\' [\"ntbrafv]
  regex string-simple-char = (any char except '\\' '"' '\n' '\t' '\b' '\r' '\a' '\f' '\v')

  regex string-char = string-simple-char | string-escape-char | unicode-char | newline

  regex string-elem = string-char | '\' newline whitespace* string-elem

  token char = ' char-char '
  token string = " string-elem "

  regex raw-string-char = (any char but ")
  regex raw-string-elem = " raw-string-char* " | '|' raw-string-content '|'

  token raw-string = r raw-string-elem

Numbers
=======

Numbers can be written in several ways::

  regex decdigit = [0-9]
  regex hexdigit = digit | [A-F] | [a-f]
  regex octdigit = [0-7]
  regex bindigit = [0-1]

  regex dec-nat  = 0 | [1-9] (_ | decdigit)*
  regex hex-nat = 0 x (_ | hexdigit)+
  regex oct-nat = 0 x (_ | octdigit)+
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

**********************
Basic grammar elements
**********************

*************
Scoping rules
*************

***********
Expressions
***********

Definition expressions
======================

*******************************
Type definitions and signatures
*******************************

****************
Units of Measure
****************

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
  -- square2 : {Multiply a ->} a -> a -> a

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

Just like arbitrary expressions can be annotated with type variables, they can
be annotated with units of measure too::

  let ballSpeed = 10 : Int [m/s]
  let zero = 0.0 : [..]
  -- zero : {Floating a ->} a ['u]

*******
Grammar
*******

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
    name <- getString                    name <- getString;
    let msg = "Hi "                      let msg = "Hi " in
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
    1 | 2 -> x        1 | 2 -> x;
    _ -> y            _ -> y;
                    }

Faux tokens
===========

::
  token $in
  token $begin  -- corresponds to {
  token $end    -- corresponds to }
  token $sep    -- corresponds to ;

Grammar rules with faux tokens
==============================

****************
Implicit modules
****************

We allow for local defaulting for implicits::

  -- (>) : {Ord a ->} a -> a -> a

  let speedCmps = do
    let default BytecodeSpeedOrd : Ord Bytecode
    assert (fastCode > slowCode)

  let sizeCmp = do
    let default BytecodeSizeOrd : Ord Bytecode
    assert (fastCode < slowCode)

******
Optics
******

Some amount of built-in support for optics?
