Blog posts related to Kentucky mule:

#. `Can Scala have a highly parallel typechecker? 
   <https://medium.com/@gkossakowski/can-scala-have-a-highly-parallel-typechecker-95cd7c146d20>`_
#. `Kentucky Mule — limits of Scala typechecking speed
   <https://medium.com/@gkossakowski/kentucky-mule-limits-of-scala-typechecking-speed-6a44bd520a2f>`_

Also see comments in this `Github issue <https://github.com/gkossakowski/kentuckymule/issues/6>`_.

Video related to Hack (parallel typechecker for FB's PHP dialect), which
as an inspiration for Kentucky Mule.
https://www.youtube.com/watch?v=uXuYVUdFY48&t=346s

Key takeaways:

#. Understanding data dependencies is very important. Hack gets away with
   type-checking function bodies in parallel because it doesn't do function
   level inference.
#. For performance, one may have to inline/duplicate stuff. For example,
   Hack inlines declarations of superclasses into subclasses.
#. One needs to carefully think about invariants that are needed. Josh talks
   about an issue where they didn't have the guarantee that if a write returns,
   then subsequent reads are successful (this can happen if multiple threads
   are trying to write and one returns while it is actually the second one that
   is still writing to the table), which caused hard-to-track-down issues.

Thoughts on Miuki
-----------------

#. I really do NOT want to skip over having function level inference.
   If you jump into code that is way too complex for you/are having a hard
   time understanding the type-checker, a good way to get things to work
   is to delete function signatures and fill inferred ones.
   
   However, one thing that we could do (and probably should) is assume that
   most functions are annotated with *correct* signatures. This means that
   dependencies need not be blocked on the definition being type-checked.
   In case there are two type errors, one from the definition and one from
   a caller, then we can try to infer the type of the definition and issue
   a better error message (whether the signature needs to be changed).

   Look at SHErrLoc for details.

   One big wrinkle with this plan is that complex signatures will usually
   involve a bunch of implicits and OCaml/Scala do not infer implicit
   parameters...

#. There are a couple of options for parsing operators

   #. GHC-style tree rebalancing - this has the benefit that files can be
      parsed in parallel without dependencies across files. Since the renaming
      stage already depends on the parsing stage, this implementation is
      potentially simpler. This also allows more flexibility to the user of
      being able to define operators anywhere.
   #. One-shot parsing - this has the benefit of not have to perform a large
      number of tree rotations at the cost of introducing dependencies at the
      parsing stage (typical Haskell code has a lot of operators and I'm
      guessing typical Miu code will too). In effect, parsing will split up
      into two phases:

      #. P1: Import + operator definition parsing.
      #. P2: Module body parsing.
      
      P1 can be done in parallel across all files. Work on P2 can be started
      for a file once P1 has finished for files corresponding to imports. Note
      that this doesn't create a problem even if we allow cylic dependencies
      between files.

      It would be nice to benchmark the difference, but it would probably take
      a substantial amount of effort to do so...
