The Miu Programming Language (美雨)
###################################

Prerequisites
=============

1. `Rust (rustup + cargo) <https://www.rust-lang.org/en-US/install.html>`_
   (compiler & tools, recommended)
2. `Haskell (stack) <https://docs.haskellstack.org/en/stable/README/#how-to-install>`_
   (build system, recommended)
3. Python3 (glue, you probably already have it)
4. `rst2pdf <https://github.com/rst2pdf/rst2pdf#installation-and-use>`_ (docs, optional)

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
