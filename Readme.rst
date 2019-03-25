The Miu Programming Language (美雨)
###################################

Prerequisites
=============

1. `Rust (rustup + cargo) <https://www.rust-lang.org/en-US/install.html>`_
   (compiler & tools, recommended)
2. `Haskell (stack) <https://docs.haskellstack.org/en/stable/README/#how-to-install>`_
   (build system, recommended)
3. Python3 (glue, you probably already have it)
4. A C/C++ compiler (used to compile the parser).
5. NPM + nodejs (needed by ``tree-sitter``) to generate the parser. This isn't
   needed if you don't work on the parser as the generated code is committed.
6. `rst2pdf <https://github.com/rst2pdf/rst2pdf#installation-and-use>`_ (docs, optional)

If you're having trouble installing any of these, please feel free to ask for
help on the issue tracker (that reminds me: I should set up some IM thingy...).

Build instructions
==================

::

  git clone https://github.com/theindigamer/miu.git
  cd miu
  # This will install the build system `sanna`
  stack build && stack install
  # navigate to a subproject, e.g. `cd miuki`
  cd miuki
  sanna build

Contributing
============

See `Contributing <https://github.com/theindigamer/miu/blob/master/.github/Contributing.rst>`_.
It is a bit sparse at the moment, please ask on the issue tracker for help if
you're stuck with something.
