Some requirements are -

* Allow precise GC
* Allow specialization (if the user is willing to give up polymorphic recursion
  and separate compilation)

Lowest three bits -
  * xx0 -> Int/Nat
  * 001 -> Pointer to GCed value
  * 101 -> Maybe static data, pointers to Miu objects in C heap, bytearrays,
           records with only unlifted types (bytearray like), or some
           combination thereof.
  * x11 -> Object header

Minimum object size = 1 word

There are two kinds -
* ``Type``: I might contain pointers that the GC needs to inspect::

    +------------+-------------+--------------------+------------------+
    | Class      | Description | Examples           | Size             |
    +------------+-------------+--------------------+------------------+
    | Primitive  |             | ``Int``, ``Nat``   | 1 word           |
    +------------+-------------+--------------------+------------------+
    | Zero-sized |             | ``Unit``           | 0 words          |
    +------------+-------------+--------------------+------------------+
    | Composite  | Has header  | ``MkPair Int Int`` | N words (N >= 2) |
    +------------+-------------+--------------------+------------------+

* ``PrimType``: I do not contain pointers that the GC needs to check::

    +-----------+-------------+-----------------------------------+------+
    | Class     | Description | Examples                          | Size |
    +-----------+-------------+-----------------------------------+------+
    | Primitive |             | ``Int64#``, ``Int32x4#``, ``U8#`` | Any  |
    +-----------+-------------+-----------------------------------+------+
    | Composite | No header   | ``MkPoint# Int16# Int16#``        | Any  |
    +-----------+-------------+-----------------------------------+------+

  We can have a built-in type constructor ``Array# : PrimType -> Type`` representing an
  a pointer to an array of primitive values::

    inBounds i a = 0 <= i && i < size a
    size : PrimArray t -> Nat
    empty : {| a : PrimArray t | size a == 0 |}
    replicate : (n : Nat) -> t -> {| a : PrimArray t | size a == n |}
    read  : (i : Nat) -> {| a : PrimArray t | inBounds i a |} -> t
    write : (i : Nat) -> t
          -> {| a : PrimArray t | inBounds i a |}
          -> {| a : PrimArray t | inBounds i a |}

NOTE: Passing type sizes across module boundaries *implicitly* breaks separate
compilation. In C/C++, if you want to access a struct in an unboxed fashion,
you need to write the implementation in a header -- this is essentially the same
problem.

Memory layout of objects (inspired by that of OCaml & Sixten)::

  -
                                                   +--- 2-bit "IsHeader" tag
                                                   v
  +----------------------+-----------+-----------+----+----------+--------------+-----
  |       metadata       |  tag byte | GC colour | 11 | value[0] |   value[1]   | ...
  +----------------------+-----------+-----------+----+----------+--------------+-----
   <-20 bits or 52 bits-> <--8 bit-->  <-2 bit->

Pointy = contains 1 or more pointers,
PF = pointy fields, PA = pointy arrays
NPF = non-pointy fields, NPA = non-pointy arrays

Different tag options:
* Ordinary tag for a sum type
* Polymorphic variant tag
  + Object size: metadata (including header)
  + Next word is unique ID for variant name (probably a hash result).
  + Fields are stored in the body (no additional indirection unlike OCaml).
* Fatpointer to array
  + Object size: 2 words (including header)
  + Next word is pointer to array
  + metadata is interpreted as array size
* NPF NPA (a.k.a. bytearray-like)
  + Object size: metadata (including header?)
* PF NPF NPA
  + Metadata interpreted as pair of sizes for PF and NPF + NPA respectively
    (how many bits each?)
* PF PA NPF (or PF PA):
  + Metadata interpreted as pair of sizes for PF + PA and NPF respectively
    (how many bits each?)

Closures
========

For small values, we should probably copy them into the object itself, and
for large values, capture them by reference.

Q: What about partial application?

Lazy values
===========

Not sure if we can copy OCaml's design - I'd strongly prefer that things be
thread-safe by default.

Polymorphic fields
==================

* We can pass sizes like Sixten for layout.
  For higher-kinded type variables, we pass a closure that computes the size
  (again like Sixten).
* If the kind of the type variable is ``Type``, then the polymorphic field is
  fitted into the pointy-space. If the kind is ``PrimType``, then it is fitted
  into the non-pointy space.

  Q: What about user-defined kinds?

Existential types
=================

Maybe attaching a vtable pointer is sufficient? Do we need to mess with the tag
byte?

One problem is that because of offset computation, "upcasting" would involve
creating a copy of the data with an update vtable pointer. For example, consider
the following made-up Haskell types

    type X = exists a. (Foo a, Bar a) => a
    type F = exists a. (Foo a) => a
    type B = exists a. (Bar a) => a
    f (a : F) = foo a
    g (a : B) = bar a
    h (a : X) = (f a, g a)

One can easily make a symmetry argument (invariance under name change) to point
out that both ``f`` and ``g`` get ``foo`` and ``bar`` respectively from offset 0
in the vtable. Hence, we need to perform at least 1 data copy when calling
``f`` or ``g``, depending on how the vtable is ordered.

Higher-rank types
=================


Calling convention
==================

One problem with all the size passing is increased register pressure.
We might want to follow a ghc/ocamlc style calling convention where there
are no callee-save registers. Or try some other calling convention.

Aside: Is it possible to design something (a pragma or otherwise), which forces
conversion of recursion to iteration in the target code, enabling us to have
useful stack traces? I should investigate what the ghc/ocamlc debugger does.
