* When a type mismatch is detected -

  1. Look for other variables in scope with a type that might fit (type-wise)
  2. Sort the suggestions using a string distance

  Note that Step 1. is also something that will be needed by `miumiugo` but at
  a different scale.
