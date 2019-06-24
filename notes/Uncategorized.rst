Key influence:
* Titus Winters, "C++ as a Live at Head Language" CppCon 2017 (`YouTube <https://www.youtube.com/watch?v=tISy7EJQPzI>`_)

(TODO: I should write a summary of the talk.)

Packages
========

We have a 3 tier system like Hackage, Stackage nightly and Stackage LTS.

Some key things that a package page should show:

* API diff: Generated automatically.
* Changes in functions transitively calling ``unsafeOk`` (or equivalent)
  between versions.

First-time publishing
---------------------

Unlike crates.io/hackage/npm/most package repositories, publishing a package
for the first time needs to go through a review process.

* Zero name-squatting tolerance policy.
* You cannot publish "micro-packages" like "is-red" or similar.
* If you're taking up a common name, there is a higher bar. For example, if you
  publish a package called ``graphql``, it is expected that you're providing
  fairly complete coverage of how a user might want to interact with a GraphQL
  API. If you don't have a fairly complete implementation, then you can use a
  unique name (e.g. ``graph-qualia``), which has a lower bar.

Subsequent versions of the same package do not need to go through such review.

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

Professional conduct
====================

A Code of Conduct is a fairly low bar for what constitutes acceptable behavior.

We should incorporate language in the CoC or add a "Code of professional conduct"
that is broader. For example, if someone shares a talk video, it is not
appropriate to comment on people's physical appearance, or make derogatory
remarks about speech or body language.

Some examples:

* "I was so distracted because the speaker is so cute." is not an appropriate comment.

* "I'm having difficulty understanding the speaker at 3:30, could someone explain
  what is being said?" is okay.

* "The speaker's thick accent makes it impossible to understand what is going on."
  is not appropriate.

First offence - warning. Second offence - 1 month suspension (say). Third offence - ban.

Q: How should microaggressions like saying "The speaker has really good English."
   for (say) an Asian speaker be handled?

Code of ethics
==============

(The section title "Code of ethics" is a placeholder. I'm not really sure what
 this looks like, but I do not want the project to cooperate with particular
 potential users. I don't know where we will draw the boundaries. I don't know
 how we will draw the boundaries. But I would be, for example, be extremely
 uncomfortable with Miu being used for military applications.
 However, the fact that it is hard, if not impossible to draw exact boundaries,
 should NOT prevent us from ruling particular things out.)

ACM `code of ethics <https://ethics.acm.org/code-of-ethics/>`_

Ravelry (knitting community) has a `No Trump policy <https://www.ravelry.com/content/no-trump>`_

Bryan Cantrill's talk on ethical dilemmas: https://www.youtube.com/watch?v=0wtvQZijPzg

Unlike the ACM code, the Ravelry policy is much more concrete, with examples
of behavior that are unacceptable.

Non-goals
=========

Being highly opinionated about style
------------------------------------

1. The compiler shouldn't be scolding people for "bad style".

2. The core tools should be somewhat configurable. Ideally, they should be
   designed as wrappers around a library. However, that isn't an excuse
   for having bad defaults. We should strive to have good defaults for all
   tools, such as linters and formatters.

Some of this is social too. People shouldn't be criticized for turning off
compiler/linter warnings or using a different style if they feel more
comfortable with that. A soft suggestion or hint is fine, but they should be
allowed to write their code the way they want to.

We can certainly provide API guidelines, but not following them shouldn't
be a big deal. These are guidelines, not commandments.

A "fits in your head" language
------------------------------

1. It is perfectly fine if teams pick subsets of the language that they want
   to stick to, for ease of onboarding or similar. However, if they're doing
   this due to problems with tools (e.g. bad compile times), that should be
   communicated and taken as a point of concern.

   The boundaries should be set by people voluntarily, not due to limitations
   of the tools they have.

2. It should be easy to know where to look/who to ask for a given piece of
   information. Accessibility, communication and transparency are valued.

However, this isn't an excuse for having a hard-to-learn language. I think it
would be valuable to have a standard library for on-boarding people relatively
new to FP that exercises fewer features, to make the learning curve more gentle.

Since modules are so fundamental, it might even be helpful to have the "starter
library" be a specialized version of the standard library.
