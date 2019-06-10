Key influence:
* Titus Winters, "C++ as a Live at Head Language" CppCon 2017 (`YouTube <https://www.youtube.com/watch?v=tISy7EJQPzI>`_)

Packages
========

We have a 3 tier system like Hackage, Stackage nightly and Stackage LTS.

Some key things that a package page should show:

* Changes in functions transitively calling ``unsafeOk`` (or equivalent)
  between versions.

Versioning
==========
* Versioning is hard to do manually, let's automate as much as possible.
* Want to avoid out-of-band information (e.g. Rust editions, C++14).
* Dependencies should be explicit for both libraries and applications,
  including dependencies on language/compiler versions.
* What language should we use to talk about dependencies?
  * Can we use (a subset of) Nix? Can we create a typed alternative to Nix?
  * Can we use a subset of Miu?
  * Can we write an EDSL in Miu? Can we write a sublanguage in Miu?

Tentative versioning scheme:

SUPER.MAJOR.AUTOFIX.MINOR.PATCH

SUPER version can be used for "marketing" etc.

AUTOFIX version = breaking changes which can be automatically be fixed

Other version numbers work as expected.

Tools
=====

We should have tools for

* semver checking
* automated/assisted migration
* changelog generation

before we hit 1.0.0.

The autofix tool MUST be runnable on the entire open source package index in
reasonable time (e.g. we should expect to run this once every week at the
minimum). This means we can create "tentative releases" after applying the
autofix, which the library author can approve with a click.
And maybe send a pull/merge request automatically...

Note: Watching Jon Cohen's CppCon 2017 talk
`A Type, by Any Other Name <https://www.youtube.com/watch?v=ely_hVVZjEU>`_
is highly recommended. It talks about doing non-atomic refactoring like the
ones we expect to do with an autofix tool.

Security
========

Compile time IO
---------------

For one reason or another, a package might want to do IO at compile time.
In most cases, this shouldn't be a thing, an effectful value should be exposed
instead. For example, if you want to provide a data table loaded from a large
file, it should be exposed as (strawman syntax)::

    let myDataTable : [IO, Abort] DataTable
    let myDataTable = abortIfError (parseIntoDataTable (loadCSV "data/table.csv"))

Then an application developer can always use::

    static dataTable = runStatic myDataTable

The application developer can blacklist the transitive use of ``runStatic`` in
dependencies, with optional whitelisting for specific versions of specific
packages.

This idea is called "compilation safety" in the Safe Haskell documentation.
https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/safe-haskell.html

Commercial users
================

* Companies that need tools to be sufficiently flexible so that they work with
  the company's code base are expected to contribute back to the tools.

* If they have closed source code where non-automatable breaking changes are
  a concern, they should run "analysis scripts" so that we can understand how
  much of an impact the changes should have. If you cannot run a script,
  your code will not be weighed when considering the impact of breaking changes.
