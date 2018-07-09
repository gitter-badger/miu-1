Typical number of top-level declarations in a Haskell file::

  rg -c "::|data|type|newtype|instance" | \
  awk -F ":" '$1~/.hs/{sum += 1; total += $2} \
  END{print "total:" total, "files:" sum, "avg:" (total/sum)}'

The numbers are ~71 for ``containers/Data`` and ~85 for ``ghc/compiler``.

Similarly for imports statements, the numbers are: ~10 for ``containers/Data``
(this seems awfully low) and ~21 for ``ghc/compiler``.

TODO: Make a table here and use other repos like Yesod/Opaleye.
