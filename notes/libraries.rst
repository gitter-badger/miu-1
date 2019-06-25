I think the default Monoid for Maybe should be like First as described by
McBride here:

https://www.reddit.com/r/haskell/comments/30s1t2/proposal_make_semigroup_as_a_superclass_of_monoid/cpvdco1

Base (standard library)
-----------------------

Base.Prelude - ...

Base.Compactable - Compactable
  See https://hackage.haskell.org/package/compactable-0.1.2.3/docs/Control-Compactable.html
Base.Foldable - Foldable, Traversable
Base.Foldable.Newtypes -
Base.Foldable.Mono - MonoFoldable, MonoFilterable, MonoTraversable, MonoWitherable
Base.Functor - Functor, Apply, Applicative, Alt, Alternative, Bind, Monad, MonadPlus
  - ``some`` -> ``oneOrMore``, ``many`` -> ``zeroOrMore``, ``optional`` -> ``zeroOrOne``
Base.Functor.Newtypes -
Base.Functor.Mono - MonoFunctor
Base.Bifunctor - ...
Base.Bifunctor.Newtypes -
Base.Profunctor - ...
Base.Profunctor.Newtypes -
Base.Semicategory - Semicategory, Category, Arrow
Base.Semigroup - Semigroup, CommutativeSemigroup, Monoid, CommutativeMonoid
Base.Semigroup.Newtypes -
Base.Semiring - Semiring, CommutativeSemiring, Ring, ...
Base.Semiring.Newtypes -
Base.Comparison - PartialEq, Eq, PartialOrd, Ord
Base.Comparison.Newtypes -

? Base.Numeric -
Base.Natural - Nat64, Natural, ...
Base.Natural.Newtypes -
Base.Integer - Int64, Integer, ...
  - tentative operators: + = safe add, +? = checked add, +! = trapping add,
                         +^ = saturating add, +& = wrapping add
Base.Integer.Newtypes -
Base.Float   - Float64, Decimal, ...
Base.Float.Newtypes -
Base.FixedPoint - ...
Base.Bits - bitwise operations

Base.Bool -
Base.Maybe -
Base.Result -
Base.Tuple -
Base.List - List
Base.Array - Array, MutArray
Base.Array.Primitive - PrimArray, MutPrimArray
Base.Vector - Vector, MutVector
Base.Vector.Primitive - PrimVector, MutPrimVector
Base.Debug - We should use a data type like Harry Garrood uses in purescript-debug
  https://github.com/hdgarrood/purescript-debugged/blob/master/src/Data/Debug/Type.purs
Base.Format -
? Base.Format.Parse -
Base.Format.Display -
 -- we use Char = EGC because otherwise we need to plumb locales everywhere...
Base.Char - Char, AsciiChar, ScalarValue, CodePoint
Base.String - String
Base.String.OmgWtf8 - C.f. https://github.com/kennytm/omgwtf8
Base.Bytes - Byte, Bytes
Base.Stream -
Base.Stream.Bundle -

Base.Conversion
Base.Generics

Base.FFI.CString -
Base.FFI.OsString -

? Base.System.Files - FilePath, ...
? Base.System.IO -
? Base.System.Process -
? Base.System.Environment -
? Base.System.Time -
  Should this be moved out? It should contain tzdata etc.
? Base.System.Signal -

Base.Effects -
Base.Lazy -

Base.Unsafe -

Containers
----------

Containers.Hash
Containers.Sequence
Containers.Queue
Containers.Heap
Containers.OrdSet - OrdSet, MutOrdSet
Containers.OrdMap - OrdMap, MutOrdMap
Containers.HashMap - HashMap, MutHashMap
Containers.HashSet - HashSet, MutHashSet

-- Maybe include some graph structure too? Or should that be a separate library?
