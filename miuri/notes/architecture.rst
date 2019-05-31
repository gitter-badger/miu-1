# Compiler users

(What about macro expansion?)

* Autofixer
  - Needs: potentially everything
    (from concrete syntax tree to type-checked output)
* Formatter
  - Needs: concrete syntax tree and operator fixities.
* Linter (e.g. HLint)
  - Needs: name-resolved syntax tree and pragmas (e.g. "disable lint 57").
* Documentation generator
  - Needs: comments and name-resolved syntax tree (for cross-linking).
* Search/Indexer (use Kythe?)
  - Needs: comments + typed tree
* Build system
  - ???
* Package manager
  - ???
* IDEs
  - `Works on 1 project at a time <https://is.gd/alNJGG>`_, unlike the compiler
    which will work on 1 package at a time.
  - Needs both concrete syntax, as well as type-checked output.
  - See the Rust RFC for <libsyntax2 https://github.com/matklad/rfcs/blob/libsyntax2.0/text/0000-libsyntax2.0.md#ide-support>`_
  - Can the compiler expose APIs that work with an IDEs implementation of a
    virtual file system, avoiding duplication of work?
    https://www.jetbrains.org/intellij/sdk/docs/basics/virtual_file_system.html
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
