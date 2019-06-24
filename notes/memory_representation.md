# Runtime representation of Miu values and types

## Guiding principle

Avoid indirection. In some cases, it is unavoidable, such as laziness or dynamic
dispatch. As far as possible, understanding block layout should not require
chasing a pointer stashed inside the block.

## Terminology

* repr = type representation. While we could just as well say "size" as that is
  the implementation we're planning to use (at least, for most types),
  the detail that repr = size is not really relevant. The only important thing
  is that we're able to compute composite representations out of primitive ones
  at runtime.
* value of kind `k` = this is shorthand for "value of some type `t` of kind `k`"

## Requirements

* Allow precise GC
* Allow specialization to varying extents
  - full specialization if the user is willing to give up polymorphic recursion
    and separate compilation
  - partial specialization with fine-grain control (e.g. specialize the
    call graph of this function)

NOTE: package/module documentation should make it clear whether the
corresponding module permits specialization or not.

NOTE: Passing reprs across module boundaries *implicitly* breaks separate
compilation. In C/C++, if you want to access a struct in an unboxed fashion,
you need to write the implementation in a header -- this is essentially the same
problem. This means that if people hand-write interfaces (to take advantage of
separate compilation during development), but forget to write down the modules
for the reprs, we need to fall back to boxing like OCaml. TODO: I need to think
this through fully. We should be able to seamlessly work

## Word identification

For simplicity, let's say we have a tricolor GC that goes through the words
and is trying to mark things. Every word is in one of four possible states,
based on its lowest three bits -

* xx0 → Int/Nat
* 001 → Pointer to GCed block.
* 101 → Pointer to non GCed "thing". The "thing" could be static data, a Miu
  block in C heap, a bytearray, a record with only primitive types
  (bytearray like), or some combination thereof.
* x11 → Block header. The remaining bits should be inspected to understand the
  block layout.

Aside: We could special case Maybe Int/Maybe Nat/Maybe (Ref a) to
take only 1 word, with 0...01 representing Nothing.

A block may be stack allocated (using `alloca` if the repr is determined
at runtime), or heap allocated.

### Potential issues

Having unboxed fields + copy semantics might create too many data copies.
A "standard" solution to this problem would be having some kind of
pointer/reference types.

However, if you have an internal reference with a plain pointer, you can't know
if an object is alive by only stepping through all the pointers you find, some
extra book-keeping is needed. An alternative to this approach is having
fat pointers which consist of a header pointer and an offset, but that creates
a problem of memory usage (1 extra word per pointer).

## Block layout

Minimum block size = 1 word (?)

### The two kinds - Type and PrimType

There are two built-in kinds -
* `Type` → I might contain pointers that the GC needs to inspect.
  Alignment = 1 word.

  | Class      | Description | Examples         | Size             |
  |:----------:|:-----------:|:-----------------|:-----------------|
  | Primitive  |             | `Int`, `Nat`     | 1 word           |
  | Zero-sized |             | `Unit`           | 0 words          |
  | Composite  | Has header  | `MkPair Int Int` | N words (N >= 2) |

* `PrimType` → I do not contain pointers that the GC needs to check.
  Alignment <= 1 word.

  | Class     | Description | Examples                    | Size |
  |:---------:|:-----------:|:----------------------------|:-----|
  | Primitive |             | `Int64#`, `Int32#`, `Nat8#` | Any  |
  | Composite | No header   | `MkPoint# Int16# Int16#`    | Any  |

### Potential issues

#### How are user-defined kinds handled?

I certainly want to allow them...

#### How is mutability handled?

Ideally, we will have something like linear/affine types to handle mutability.
It doesn't seem to make much sense to write things here before we have a good
model for the type system...

#### What about things that need alignment > 1 word?

This [question](https://stackoverflow.com/q/28898277/2682729) indicates that
some data types need alignment more than the word size. One example is SIMD
vectors, say `Int32x4#`. This messes things up for types of kind `Type`,
because we can't just proceed with alignment = word size if we want to allow
these :( :( :(. In theory, there's nothing stopping us from creating two more
kinds:

```
                          --> = can be contained in

              PrimType       --------------->       Type
    (size = n bytes, align <= 1 word)  (size = n words, align = 1 word)
                 |           \                       |
                 |            +----------------+     |
                 v                              \    v
            WidePrimType     --------------->     WideType
    (size = n bytes, align > 1 word)   (size = n words, align > 1 word)
```

but this creates an even bigger headache if we want both composability and
efficiency :(

#### FFI cost

For languages like OCaml and Haskell, FFI can be expensive if types are
getting converted across the boundary. Can we avoid this, as well as
have good ergonomics? For example, if you look at
[ocaml cstruct](https://github.com/mirage/ocaml-cstruct),
it doesn't look very ergonomic as one has to use getters and setters.

#### Separate compilation for PrimType

TODO: Add some information on how we handle separate compilation with types of
kind `PrimType`; it be possible, otherwise people will actively avoid using
`PrimType`. One possible solution (I think?) is that we pass alignment along
with the size specifically for `PrimType`.

For example, we could add alignment information in the top bits. Computing the
new size and new alignment on combination can be done with a few instructions.
Sixten used this idea initially.

Related: TODO[Polymorphism with PrimType]

#### GC roots from registers <-> PrimType ?

The GC needs to scan the stack and registers for GC roots. For stack values,
we can "segregate" values of kind `Type` and `PrimType` for each stack frame,
so the GC only scans values of kind `Type`. However, what do we do about the
values stored in registers? GHC already solves this problem somehow, because
you can pass arbitrary `Int64#` values in registers - I should look at how
they do it.

#### Code duplication/less code reuse

**Bad:**
Type variables default to kind ``Type``. This means that a bunch of code
may need to be reimplemented for values of kind ``PrimType`` with little to
no changes.

**Good:**
There are different "best" solutions in monomorphic and polymorphic contexts.
If everything is monomorphic, then using types from ``PrimType`` means you
have less overhead/more flexibility. However, if you suddenly want to use
a polymorphic function, then you need to use a type from ``Type``.
(TODO[Polymorphism with PrimType]: Add some examples here)

### Block layout details

Memory layout of blocks (inspired by that of OCaml & Sixten):

```
                                               +--- 2-bit "IsHeader" tag
                                               v
  +------------------|-----------|-----------|----|----------|--------------|-----
  |     metadata     |  tag byte | GC colour | 11 | value[0] |   value[1]   | ...
  +------------------|-----------|-----------|----|----------|--------------|-----
   <----52 bits-----> <--8 bit--> <--2 bit-->
```

The block contains a 64-bit header (let's assume it is the same on 32-bit
systems), which is divided as shown above. The overall size is some multiple of
the word size, including padding as necessary (the diagram doesn't show any
padding). The space taken up by each value need not be a multiple of the word
size, due to the presence of values of kind `PrimType`.

```
type F (a# : PrimType) : Type = { first : a#, second : a# }
-- Say at runtime, we get the info, that size of a# is 4 bytes, and the
-- alignment is 2 bytes. Can we compute the size of F a# and the offsets
-- for each field? Yes! We basically run the functions that the compiler
-- would run at compile time, but run them at runtime instead! The only
-- difference is that computing a highly optimized layout might be too
-- expensive at runtime, because we need to do so repeatedly.
```

The next question is: what are the possible values for the tag byte and metadata,
and what values can we put in the body of the block?

#### The tag byte and metadata fields

A data type is either:

* a built-in
  - `Ref a` (fat pointer to block)
* a newtype, i.e., a sum with only 1 constructor or a product with only 1 field → a newtype shares the layout of the type
* a closed sum type with 2 or more constructors
* a closed product type with two or more fields
* an open sum type (polymorphic variant from OCaml)
* an open product type (extensible records like Purescript)

the different possible values

Pointy = contains 1 or more pointers,
PF = pointy fields, PA = pointy arrays
NPF = non-pointy fields, NPA = non-pointy arrays

Different tag options:
* Ordinary tag for a sum type (251 / 256)
  + TODO: How to interpret metadata? Special casing for inline records (or equiv.)?
* Polymorphic variant tag (1 / 256)
  + Block size: metadata (including header)
  + Next word is unique ID for variant name (probably a hash result).
  + Fields are stored in the body (no additional indirection unlike OCaml).
* Fatpointer to block (1 / 256)
  + Block size: 2 words (including header)
  + Next word is pointer to block
  + metadata is interpreted as offset in block
* NPF NPA (a.k.a. bytearray-like) (1 / 256)
  + Block size: metadata (including header)
* PF NPF NPA (1 / 256)
  + Metadata interpreted as pair of sizes for PF and NPF + NPA respectively
    (how many bits each?)
* NPF PF PA (1 / 256)
  + Metadata interpreted as pair of sizes for NPF and PF + PA respectively
    (how many bits each?)

#### Open sums and products via row types

```
Containment translates to an offset array.
Combination translates to a boolean array.

(λr. r.x) : forall t z. (| x ◃ t |) ⊆ z => Record z -> t
-->
(λr. r.x) : Repr "t" -> Repr "z" -> OffsetArray [("x", "z")] -> Record z -> t

z 1 ⊙ z 2 ∼ z 3 --> Array is-z1-or-z2

f : {x : int, y : int | r} -> int
let p = {x = 10, y = 20, z = 30}
let x = f @x.offset @y.offset p

```

#### Different built-in arrays

Example: We can have a built-in type constructor `PrimArray : PrimType -> Type`
representing an a pointer to an array of primitive values::

```
inBounds i a = 0 <= i && i < size a
size : PrimArray t -> Nat
empty : {@ a : PrimArray t | size a == 0 @}
replicate : (n : Nat) -> t -> {@ a : PrimArray t | size a == n @}
read  : (i : Nat) -> {@ a : PrimArray t | inBounds i a @} -> t
write : (i : Nat) -> t
     -> {@ a : PrimArray t | inBounds i a @}
     -> {@ b : PrimArray t | inBounds i b @}
```


#### Inline arrays - PITA or very cool?

```
type Foo = { x : forall l. InlineArray# l Int64# }

class Shape ( where

sizeOf : InlineArray#

```

NOTE:
  We should be careful about how inline arrays are handled in types. After
  all, we cannot create an unboxed array if different values of the same type
  have differently sized inline arrays.

  The most likely type design is to have something like
  `InlinePrimArray : ^Natural -> PrimType -> PrimType`. The `sizeOf`
  function needs to reify the first type argument to get a natural number.

  How exactly this should be handled in the type system needs more thought,
  so that it isn't possible to create an misshapen unboxed array, without
  introducing weird rules. If this is too tricky, we can skip inline arrays
  and only provide two type constructors ``PrimArray`` (as earlier) and a
  similar `Array : Type -> Type`.

Examples::

```
type A : Type = MkA Int64#
MkA 7 => tag = npf_npa_tag, metadata = 2, value[0] = 7

type A' : Type = { a : Int64# }
{a = 7} => tag = npf_npa_tag, metadata = 2, value[0] = 7

type Either : Type = L A | R A'
L (MkA 7)  => tag = 0 (say), metadata = 3 (say?),
              value[0].words[0].tag = npf_npa_tag,
              value[0].words[0].metadata = 2
              value[0].words[1] = 7

type Either : Type = L {a : Int64#} | R Int64#
L (MkA 7)  => tag = 0 (say), metadata = 3 (say?),
              value[0].words[0].tag = npf_npa_tag,
              value[0].words[0].metadata = 2
              value[0].words[1] = 7

type IntList : Type = Nil | Cons Int64# (Ref IntList)

Nil => tag = 0 (say), metadata = 4, value[i] = garbage
Cons 10 (..) => tag = 1 (say), metadata = 4, value[0] = 10,
                value[1].words[0].meta = some offset,
                value[1].words[0].tag = fatptr_tag,
                value[1].words[1] = ptr
```

### Closures

For small values, we should probably copy them into the block itself, and
for large values, capture them by reference.

Q: What about currying?

Look at: Making a fast curry Push/enter vs eval/apply for higher-order languages

### Lazy values

Not sure if we can copy OCaml's design - I'd strongly prefer that things be
thread-safe by default.

UPDATE: This `PR <https://github.com/ocaml-multicore/ocaml-multicore/pull/226>`_
makes lazy values thread-safe. The approach there seems worth investigating.

### Polymorphic fields

* We can pass sizes like Sixten for layout.
  For higher-kinded type variables, we pass a closure that computes the size
  (again like Sixten).
* If the kind of the type variable is ``Type``, then the polymorphic field is
  fitted into the pointy-space. If the kind is ``PrimType``, then it is fitted
  into the non-pointy space.

### Existential types

Say we have an existential type like the following:

```
-- The placeholder syntax ⊗ attaches vtables to data types.
type X = exists a. (Foo a) ⊗ a
```

This is essentially an object type. The memory representation for this should
probably have 3 words + the header.

* the first word is `a`'s repr.
* the second word is a pointer to a `Foo a` module
* the third word is a pointer to `a` itself

#### Possible issues

Unlike OO languages, "upcasting" involves copying data because the struct size
shrinks. Even if we use a pointer to an array of pointers to modules (an
unflattened vtable) to avoid this, the offsets will change depending on which
module is "forgotten", meaning that we still need to create a copy sometimes.

```
-- The placeholder syntax ⊗ attaches vtables to data types.
type X = exists a. (Foo a, Bar a) ⊗ a
type F = exists a. (Foo a) ⊗ a
type B = exists a. (Bar a) ⊗ a
f (a : F) = foo a
g (a : B) = bar a
h (a : X) = (f a, g a)
```

One can easily make a symmetry argument (invariance under name change) to point
out that both ``f`` and ``g`` get pointers to ``Foo a`` and ``Bar a``
respectively from offset 0 in the vtable. Hence, we need to perform at least
1 data copy when calling ``f`` or ``g``, depending on how the vtable is ordered.

### Higher-rank types

### Calling convention

All the repr passing will probably create increased register pressure.
We might want to follow a ghc/ocamlc style calling convention where there
are no callee-save registers. Or try some other calling convention.

Aside: Is it possible to design something (a pragma or otherwise), which forces
conversion of recursion to iteration in the target code, enabling us to have
useful stack traces? I should investigate what the ghc/ocamlc debuggers do.
