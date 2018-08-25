Diagrams
========

Gotchas:
--------

* The following Unicode scalars are not allowed:

  - Private use area: U+E004, U+F8FF

* Line segments should have length 3 or more (including bends, intersections,
  arrowheads, vertices). Some examples:

  - ``->`` isn't rendered as an arrow but ``-->`` is (``>`` is an arrowhead).
  - ``*--`` does render as an edge from a vertex.
  - ``++`` for a tight bend (or an intersection) doesn't work, use ``+-+``.
  - ``->*`` doesn't render as an arrow to a vertex because arrowheads/vertices
    act as terminators, so the segment ends up having length 2. Use ``-->*``.
  - ``*->`` does render as an arrow from a vertex.
