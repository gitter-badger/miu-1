Ideas
=====

* Accessibility

  - Steal ideas for color schemes from other static site generators such as
    Hugo, Gutenberg, Jekyll. At least 1 option should work well for people
    with red-green colorblindness.
  - Rustdoc has many themes, steal some?
  - See themes `here <https://tmtheme-editor.herokuapp.com/#!/editor/theme/Agola%20Dark>`_.
  - Make themes freely available on userstyles.org so that people can save
    defaults if they wish. Don't use cookies for that.

* Choice of default markup language

  - Use cases:
    + Short documentation comments - Markdown is really good here.
    + Longer tutorials written in literate style - Markdeep is really good here
      if you don't want customizability in a platform-agnostic way: just use
      HTML.
    + If people really like it and the ecosystem allows, they'll want to use it
      to create slides, notes, blogs and even books!
  - Customizability:

* From Haddock

  - Information should be sourced from the compiler.
  - Cross-linked identifiers by default (like Haddock's quotes)
  - Documentation linked to source which itself is cross-linked.

* Footprint

  - Total compressed transfer size of most (>90%) pages which have no images
    should be less than 100 kB (including fonts).
  - Very small amount of Javascript (for search only) which can be opted out of
  - Compress stuff :D
  - Convert diagrams statically into SVGs, instead of using
    JS like Markdeep (use JS if desired while developing locally but reference
    docs must not use JS).
  - Convert graphs (via Graphviz?) statically into SVGs, instead of using
    a third-party site like Gravizo.
  - Convert math statically to SVGs (see `m.css <http://mcss.mosra.cz/>`_)
    instead of using JS like MathJax.

* Usability

  - Font subsetting for special fonts used (if any)
  - Doesn't break Vimium or equivalent

* Flexibility

  - Modular over input formats - Numark (placeholder custom markup language), or
    Markdown (may not be able to support all features).
  - Modular over documentation style - reference, tutorial, book
  - Modular over presentation style - web, handout, slides
  - Can be used as a library if someone else wants to set up their own thing.
  - Internationalization

* reST Extensions

  - Ability to make diagrams (e.g.
    `Markdeep <https://casual-effects.com/markdeep/features.md.html#toc1.15>`_).
    How the hell does this work?
    See the `diagramToSVG <https://github.com/morgan3d/markdeep/blob/master/latest/markdeep.js#L3062>`_
    function.

    + Related: `HN <https://news.ycombinator.com/item?id=10290073>`_ with lots of
      options such as AsciiFlow, Mermaid, etc.
    + Possible issue: People keep wanting fancier features in diagrams.

State-of-the-art discussions for the inevitable, infinite debates:

* `RST vs Markdown <http://www.zverovich.net/2016/06/16/rst-vs-markdown.html>`_,
  [`HN <https://news.ycombinator.com/item?id=11922485>`_\ ]
* `Don't use Markdown for technical docs <http://ericholscher.com/blog/2016/mar/15/dont-use-markdown-for-technical-docs/>`_
  [`HN <https://news.ycombinator.com/item?id=11292280>`_\ ]
* `Rust internals thread <https://internals.rust-lang.org/t/rustdoc-restructuredtext-vs-markdown/356>`_

Misc:

* Bay area Rust talk: https://air.mozilla.org/rust-meetup-december-2013/
  (video can be downloaded!)
