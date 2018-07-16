# Sanna

Sanna is an internal build system for the Miuki compiler (and associated tools
in the future) written in Haskell.

Right now, it is mostly an overlay on `cargo`.

The main commands are:
```
-- rebuild sanna if you're working on sanna itself
sanna refresh

-- build the project(s) corresponding to the current working directory
sanna build
```

## Why Haskell? Why not Make/Shell/Python/Rust?

* Make - Extremely easy to write code that you can't read 1 week later.
* Shell - Cross-platform is hard. Writing subtly incorrect code is easy. Why use
  `shellcheck` when you can just use a compiler? No type system.
* Python - Why use `pylint`+`mypy` when you can just use a compiler?
* Rust - I don't know of any libraries to write build systems in Rust. We use
  the `shake` library here.

Also I like Haskell.
