# Compiler users

(What about macro expansion?)

* Autofixer
  - Needs access to potentially everything 
    (from concrete syntax tree to type-checked output)
* Formatter
  - Needs a concrete syntax tree and operator fixities.
* Linter (e.g. HLint)
  - Needs access to name-resolved syntax tree and pragmas (e.g. "disable lint 57").
* Documentation generator
  - Needs access to comments and name-resolved syntax tree (for cross-linking).
* Build system
  - ???
* Package manager
  - ???
* IDEs
  - `Works on 1 project at a time <https://is.gd/alNJGG>`_, unlike the compiler
    which will work on 1 package at a time.
  - Needs both concrete syntax, as well as type-checked output.
* External syntax highlighter (not super important)
  - Needs a concrete syntax tree

# Passes

Where does macro expansion go?

::
  Tentative compiler passes

           Text
            ↓  Lexing
       Token stream (explicit space)
            ↓  Indent-check
       Token stream (implicit space)
            ↓  Parsing
     Concrete Syntax Tree
            ↓  Name resolution
       Named Syntax Tree
            ↓  Macro expansion?
       Named Syntax Tree
            ↓  ASTGen
     Abstract Syntax Tree
            ↓  Type-check
    Decorated Syntax Tree
            ↓  IRGen
            IR
