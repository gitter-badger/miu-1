
## Accidental misuse of command line arguments

* Output flags should check that the user is not accidentally overwriting
  source code or other hand-written files using some heuristics.

  See https://www.reddit.com/r/haskell/comments/axcuyp/project_file_recovery/
  as an example.

## Error reporting

* When a type mismatch is detected -

  1. Look for other variables in scope with a type that might fit (type-wise)
  2. Sort the suggestions using a string distance

  Note that Step 1. is also something that will be needed by `miumiugo` but at
  a different scale.

## Telemetry

The compiler/build tool should support telemetry (all features off by default,
new features off by default, global (?) + package-local settings).

(Aside: If we detect that the License changed from FOSS to proprietary and
telemetry is ON, we can provide a hint for turning it OFF. Similar for OFF->ON
when changing the License from proprietary to FOSS.)

[/u/timcameronryan](https://www.reddit.com/r/rust/comments/8llfut/have_you_ever_complained_that_rustc_is_slow_we/dzilsxa) suggests a strategy of separating the data collection (done by
the compiler/build tool) from the data uploading (done by another binary), which
sounds like a good idea.

Some kind of data we can collect:

* What is the frequency of different compiler errors and warnings?
* Compile times vs complexity measures.
* Report internal exceptions thrown by the compiler.
* Report potential compiler issue where a module takes a lot more time to
  compile than one might expect based off some complexity measure.

In many cases, it would very useful to have a minimal code example that
demonstrates the issue (or almost impossible to do something without having
source), not sure how that should be handled...
