Typical number of top-level declarations in a Haskell file::

  rg -c "::|data|type|newtype|instance" | \
  awk -F ":" '$1~/.hs/{sum += 1; total += $2} \
  END{print "total:" total, "files:" sum, "avg:" (total/sum)}'

The numbers are ~71 for ``containers/Data`` and ~85 for ``ghc/compiler``.

Similarly for imports statements, the numbers are: ~10 for ``containers/Data``
(this seems awfully low) and ~21 for ``ghc/compiler``.

TODO: Make a table here and use other repos like Yesod/Opaleye.

Compilation speed targets
-------------------------

Sorbet (for Ruby in C++) type-checks at 100,000 lines/sec
[May 29, 2019 | `reddit <https://www.reddit.com/r/haskell/comments/bu7big/haskell_weekly_podcast_episode_11_profiling/ep9phqv?utm_source=share&utm_medium=web2x>`_\ ].

The DMD compiler compiles itself in < 7s for roughly 300kloc
[Nov 18, 2018 | `blog <https://blog.thecybershadow.net/2018/11/18/d-compilation-is-too-slow-and-i-am-forking-the-compiler/>`_\ ].

The Jai compiler compiles (including code gen) at ~100,000 lines/sec
[Dec 22, 2018 | `YouTube <https://youtu.be/-9c095aXc-s?t=9980>`_\ ].

I'm not sure how realistic it is to have similar compilation speeds (especially with more
sophisticated type system features) but we should at least aim for numbers within a factor
of 2.
