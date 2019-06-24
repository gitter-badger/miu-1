Project organization
********************

* ``miudoc``   - documentation
* ``miuri``    - compiler (Rust)
* ``miuhi``    - prototyping compiler (Haskell)
* ``miuspec``  - specification
* ``numark``   - markup language
* ``sanna``    - build system for all the projects (including itself)

Some other project names for the future: miumiugo (code search),
sojiro (editor services), cumiulus (package repository).

Guidelines
**********

* Avoid using the following words (and variations) in communication
  (GitHub comments etc.) and *especially* in documentation:

  - 'just'   - exception: the ``Just`` constructor (capitalization!)
  - 'simply' - exception: the verb "simplify" is okay
  - 'obviously'
  - 'clearly'
  - 'dumb'

  If you find examples of such usage, please submit a PR!

Bug reports
***********

GitHub tags
***********

Borrowed partly from rust-lang/rust :smile:.

* Yellow + A = Area of the project (e.g. doc, diagrams, parser).
* Light purple + C = Category of issue (e.g. bug, feature, cleanup).
* Green + E = Experience needed (e.g. newcomer, contributor, maintainer).
* Purple grey + O = OS/platform related to issue, if any.
* Blue + PJ = Project (e.g. miudoc, miuki)
* Orange + P = Priority level (e.g. low, medium, high, urgent).

Typically, anything with ``A-doc`` should also be:

* ``P-high`` or higher if also marked as ``C-bug``.
* ``P-medium`` or higher.
