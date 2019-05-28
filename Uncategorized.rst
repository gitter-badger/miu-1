Key influence:
* Titus Winters, "C++ as a Live at Head Language" CppCon 2017 (`YouTube <https://www.youtube.com/watch?v=tISy7EJQPzI>`_)

Packages
========

We have a 3 tier system like Hackage, Stackage nightly and Stackage LTS.

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

Commercial users
================

* Companies that need tools to be sufficiently flexible so that they work with
  the company's code base are expected to contribute back to the tools.

* If they have closed source code where non-automatable breaking changes are
  a concern, they should run "analysis scripts" so that we can understand how
  much of an impact the changes should have. If you cannot run a script,
  your code will not be weighed when considering the impact of breaking changes.
