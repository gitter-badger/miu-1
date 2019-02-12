
Some requirements are -

* Allow precise GC
* Allow specialization (if user is willing to give up polymorphic recursion
  + separate compilation)

Lowest three bits -
  * xx0 -> Int/Nat
  * 001 -> Pointer to GCed value
  * 101 -> Maybe static data, pointers to Miu objects in C heap, bytearrays,
           records with only unlifted types (bytearray like), or some
           combination thereof.
  * x11 -> Objects header

Minimum object size = 1 word

There are two kinds -
* ``Type``: I might contain pointers that the GC needs to inspect::

    +------------+-------------+--------------------+------------------+
    | Class      | Description | Examples           | Size             |
    +------------+-------------+--------------------+------------------+
    | Primitive  |             | ``Int``, ``Nat``   | 1 word           |
    +------------+-------------+--------------------+------------------+
    | Zero-sized |             | ``Unit``           | 0 word           |
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
compilation. This is a smaller version of the ABI stability problem in C++.

Memory layout of objects::

                                                   +--- 2-bit "IsHeader" tag
                                                   v
  +-------------------------+-----------+---------+----+----------+--------------+-----
  |        metadata         |  tag byte |  color  | 11 | value[0] |   value[1]   | ...
  +-------------------------+-----------+---------+----+----------+--------------+-----
   <-either 20 or 52 bits->  <--8 bit--> <-2 bit->

Pointy = contains 1 or more pointers,
PF = pointy fields, PA = pointy arrays
NPF = non-pointy fields, NPA = non-pointy arrays

Different tag options:
* Fatpointer to array
  + Object size is 2 words (including header)
  + Next word is pointer to array
  + metadata is interpreted as array size
* NPF NPA (a.k.a. bytearray-like)
  + Object size is equal to metadata (including header?)
* PF NPF NPA
  + Metadata interpreted as pair of sizes for PF and NPF + NPA respectively
* PF PA NPF (or PF PA):
  + Metadata interpreted as pair of sizes for PF + PA and NPF respectively

• some pointy-fields + non-pointy fields + non-pointy array
• some fields + pointy-array
• some pointy-fields + non-pointy fields

What about layout of closures? For small values, we should probably copy them
into the object itself, and for large values, capture them by reference.
What about partial application?
