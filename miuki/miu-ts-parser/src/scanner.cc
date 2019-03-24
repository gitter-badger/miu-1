// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#include <cstdio>
#include <tree_sitter/parser.h>
#include <vector>
#include <cwctype>
#include <cassert>
#include <cstring>
#include <climits>

#define SCANNER_DEBUG_FLAG 0

#define DEBUG(x) if (SCANNER_DEBUG_FLAG) { x; }
#define UNREACHABLE(s) printf(\
  "{ \"tool\" : \"miu-lexer\", " \
  "  \"msg\" : \"" s \
               " Consider reporting this as a bug on the issue tracker." \
               "\"" \
  "}");

// TODO: We need to have a solid strategy that handles multiline strings
// and multiline comments.
//
// For example, GHC allows the following code:
//
// main = do
//     print {-
// -} "!"
//
// Maybe the rule should be that if we're in the middle of a multiline string or
// a multiline comment, then we act as if we're at a deeper indentation, even though
// we might actually have shallower indentation.
//
// tree-sitter-python seems to be doing something pretty strange here. It
// just gobbles inline comments! So it might happen that you consumed a "NEWLINE"
// token, but when you inspect that token's string, it contains comments in
// arbitrary places!

namespace {

using std::vector;

// Ignoring comments, here is 1 example of expected behaviour.
//
// do                do, INDENT
// ··foo    Tokens   ··, foo, NEWLINE
// ··do    ------->  ··, do, INDENT
// ····bar           ····, bar, DEDENT
// ··blep            ··, blep, DEDENT
//
// We make sure that INDENTs and DEDENTs are balanced. This means that DEDENT
// tokens do not always correspond to newlines.
//
// do                do, INDENT
// ··foo    Tokens   ··, foo, NEWLINE
// ··do    ------->  ··, do, INDENT
// ····bar           ····, bar, DEDENT, DEDENT
//
enum TokenType {
    NEWLINE,
    INDENT,
    DEDENT,
};

// TODO: Blocked on upstream.
//
// For now, tree-sitter has no way of distinguishing eof from null.
// https://github.com/tree-sitter/tree-sitter/issues/280
//
// We disallow nulls in the language spec anyways (who knows what
// might crash if this were allowed), so this isn't super important.
const uint32_t MY_EOF = 0;

// The lexer->advance functions uses boolean arguments but it isn't clear what
// they're supposed to mean.
const bool IS_WHITESPACE = true;
const bool IS_NOT_WHITESPACE = false;

// Mixtures of spaces and tabs are a bad idea but we support them to avoid
// having hard-to-diagnose errors at this stage.
//
// We can return compiler warnings at a later stage.
struct IndentItem {
    uint16_t spaces;
    uint16_t tabs;

    bool operator==(IndentItem const& i) const {
        return spaces == i.spaces && tabs == i.tabs;
    }

    bool operator>(IndentItem const& i) const {
        return
          (spaces > i.spaces && tabs >= i.tabs)
          || (spaces >= i.spaces && tabs > i.tabs);
    }

    bool operator<(IndentItem const& i) const {
      return
        (spaces < i.spaces && tabs <= i.tabs)
        || (spaces <= i.spaces && tabs < i.tabs);
    }

    bool not_comparable_to(IndentItem const& i) const {
        return (spaces >= i.spaces && tabs < i.tabs)
          || (spaces < i.spaces && tabs >= i.tabs);
    }

    // IIUC, the default initializer is allowed to stuff garbage in the fields,
    // so it is better that we write it explicitly.
    IndentItem() : spaces(0), tabs(0) {}
};

struct Scanner {
    // Keep track of how many DEDENT tokens we will "emit" in the future.
    //
    uint32_t pending_dedents;

    // Indentation for the last line consumed. We need to keep track of this
    // as consuming a newline means we also consume stuff after it, and so when
    // we're checking the indent level later, we somehow need access to the stuff
    // we consumed when tree-sitter called us the last time.
    //
    // Unlike the Miu scanner, we don't just get the "codepoint index" from
    // tree-sitter as that wouldn't let us distinguish between tabs and spaces.
    IndentItem latest_indent;

    // INVARIANT: The stack is non-empty, with the first element being the
    // (0, 0) value. This means that there is no possible ItemIndent which is
    // less than indent_stack[0].
    vector<IndentItem> indent_stack;

    Scanner() {
        // DEBUG(printf("===  In constructor  ===\n"));
        deserialize(NULL, 0);
        // DEBUG(printf("=== Constructor ends ===\n"));
    }

    unsigned serialize(char* buffer) const {
        static_assert(CHAR_BIT == 8, "Expected char to be 8 bit wide.");

        size_t n_copied_so_far = 0;
        size_t n_to_copy = sizeof(decltype(pending_dedents));
        std::memcpy((void *) &(buffer[n_copied_so_far]),
                    (void *) &pending_dedents,
                    n_to_copy);
        n_copied_so_far += n_to_copy;

        n_to_copy = sizeof(IndentItem);
        std::memcpy((void *) &(buffer[n_copied_so_far]),
                    (void *) &latest_indent,
                    n_to_copy);
        n_copied_so_far += n_to_copy;

        n_to_copy = indent_stack.size() * sizeof(IndentItem);
        if (n_copied_so_far + n_to_copy > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
            UNREACHABLE("WARNING: Tree sitter's buffer is too small."
                        " This shouldn't be happening, unless your code has an"
                        " unhealthy number of indentation levels all at once.");
            n_to_copy = TREE_SITTER_SERIALIZATION_BUFFER_SIZE - n_copied_so_far;
            n_to_copy = n_to_copy - n_to_copy % sizeof(IndentItem);
        }
        std::memcpy((void *) &(buffer[n_copied_so_far]),
                    (void *) indent_stack.data(),
                    n_to_copy);
        DEBUG(printf("Copied %zu bytes for indent_stack\n", n_to_copy));
        n_copied_so_far += n_to_copy;

        return n_copied_so_far;
    }

    void deserialize(const char* buffer, unsigned length) {
        pending_dedents = 0;
        latest_indent = IndentItem();
        indent_stack.clear();

        if (buffer == NULL) {
            DEBUG(printf("Got NULL for buffer :-/.\n"));
            DEBUG(printf("Created default element to guarantee invariant.\n"));
            DEBUG(printf("After deserializing, stack size = 1.\n"));
            indent_stack.push_back(IndentItem());
            return;
        }

        size_t n_remaining = length;
        size_t n_to_copy = sizeof(pending_dedents);
        size_t n_copied_so_far = 0;

        if (n_remaining < n_to_copy) return;
        std::memcpy((void *) &pending_dedents,
                    (void *) &(buffer[n_copied_so_far]),
                    n_to_copy);
        n_remaining -= n_to_copy;
        n_copied_so_far += n_to_copy;

        n_to_copy = sizeof(IndentItem);
        if (n_remaining < n_to_copy) return;
        std::memcpy((void *) &latest_indent,
                    (void *) &(buffer[n_copied_so_far]),
                    n_to_copy);
        n_remaining -= n_to_copy;
        n_copied_so_far += n_to_copy;

        n_to_copy = n_remaining - (n_remaining % sizeof(IndentItem));
        if (n_to_copy == 0) {
            // Save an empty value to preserve the non-empty invariant.
            indent_stack.push_back(IndentItem());
            DEBUG(printf("WARNING: This case shouldn't have happened.\n"));
            DEBUG(printf("Created default element to guarantee invariant.\n"));
            DEBUG(printf("After deserializing, stack size = 1.\n"));
            return;
        }
        indent_stack.resize(n_to_copy / sizeof(IndentItem));
        std::memcpy((void *) indent_stack.data(),
                    (void *) &(buffer[n_copied_so_far]),
                    n_to_copy);

        DEBUG(printf("After deserializing, stack size = %zu.\n", indent_stack.size()));
    }

    bool scan(TSLexer* lexer, const bool* valid_symbols) {

        const bool RECOGNIZED = true;
        const bool UNKNOWN    = false;

        // TODO: What if valid_symbols[DEDENT] && pending_dedents == 0?
        //       I think that should be impossible if the grammar is written
        //       properly.
        if (valid_symbols[DEDENT] && pending_dedents > 0) {
            pending_dedents--;
            lexer->result_symbol = DEDENT;
            return RECOGNIZED;
        }

        bool consumed_newline = false;
        bool done = false;

        while (!done) {
          switch (lexer->lookahead) {
            case '\n':
              consumed_newline = true;
              latest_indent.spaces = 0;
              latest_indent.tabs = 0;
              lexer->advance(lexer, IS_WHITESPACE);
              // Now, we make sure that the current lexeme includes everything
              // up to (and including) the newline we're at.
              // It is okay to call mark_end multiple times to increase the
              // size of the lexeme, which means that this does handle blank
              // lines correctly. For example (with Unix-y line endings):
              //
              // foo··$      the lexeme will be "  \n     \n"
              // ·····$  -->
              // ···bar      the prefix "   " before "bar" will not be consumed
              //
              // N.B. The dot is U+00b7.
              lexer->mark_end(lexer);
              break;

            case '\r':
              // Next character after \r should be \n but that is not checked.
              lexer->advance(lexer, IS_WHITESPACE);
              break;

            case ' ':
              latest_indent.spaces++;
              lexer->advance(lexer, IS_WHITESPACE);
              break;

            case '\t':
              latest_indent.tabs++;
              lexer->advance(lexer, IS_WHITESPACE);
              break;

            case MY_EOF:
              if (valid_symbols[DEDENT] && indent_stack.size() > 1) {
                  indent_stack.pop_back();
                  lexer->result_symbol = DEDENT;
                  return RECOGNIZED;
              }
              if (valid_symbols[NEWLINE]) {
                  lexer->result_symbol = NEWLINE;
                  return RECOGNIZED;
              }
              return UNKNOWN;

            default:
              done = true;
          }
        }

        // TODO: Handle comments
        bool next_token_is_comment = false;

        if (!next_token_is_comment) {
            IndentItem current_indent;

            if (indent_stack.size() == 0) {
                UNREACHABLE("WARNING: Invariant violated. Expected indentation "
                            " stack to be non-empty.");
                indent_stack.push_back(current_indent);
            } else {
                current_indent = indent_stack.back();
            }

            if (consumed_newline) {
                // do
                // ··line0$
                // ··line1
                //   ^ latest_indent
                if (valid_symbols[NEWLINE] && latest_indent == current_indent) {
                    lexer->result_symbol = NEWLINE;
                    return RECOGNIZED;
                }

                if (latest_indent > current_indent) {
                    // do
                    // ··line0$
                    // ····line1
                    //     ^ latest_indent
                    if (valid_symbols[INDENT]) {
                        indent_stack.push_back(latest_indent);
                        lexer->result_symbol = INDENT;
                        return RECOGNIZED;
                    }
                    // do$
                    // ··do {$       --> consumed '\n'
                    // ····let line0
                    //     ^ latest_indent
                    if (valid_symbols[NEWLINE]) {
                        lexer->result_symbol = NEWLINE;
                        return RECOGNIZED;
                    }
                    // TODO: Add unreachable msg here.
                }

                if (latest_indent < current_indent) {
                    // do$
                    // ··do$
                    // ····line0$ --> consumed '\n'
                    // ··line1
                    //   ^ latest_indent
                    if (valid_symbols[DEDENT]) {
                        indent_stack.pop_back();
                        while (indent_stack.size() > 1 && latest_indent < indent_stack.back()) {
                            pending_dedents++;
                            indent_stack.pop_back();
                        }
                        lexer->result_symbol = DEDENT;
                        return RECOGNIZED;
                    }
                    // do$
                    // ····do {$  --> consumed '\n'
                    // ··line0
                    //   ^ latest_indent
                    if (valid_symbols[NEWLINE]) {
                        lexer->result_symbol = NEWLINE;
                        return RECOGNIZED;
                    }
                    // TODO: Add unreachable msg here.
                }

                if (latest_indent.not_comparable_to(current_indent)) {
                    // TODO: Decide a good error handling strategy here.
                    lexer->result_symbol = NEWLINE;
                    return RECOGNIZED;
                }

            }
        }
        return false;
    }
};

} // end namespace

extern "C" {

    void* tree_sitter_miu_external_scanner_create() {
        auto* tmp = new Scanner();
        DEBUG(printf("=== Create, stack size = %zu ===\n", tmp->indent_stack.size()));
        return tmp;
    }

    bool tree_sitter_miu_external_scanner_scan(void* payload,
                                               TSLexer* lexer,
                                               const bool* valid_symbols)
    {
        Scanner* scanner = static_cast<Scanner*>(payload);
        DEBUG(printf("=== Scan start, stack size = %zu ===\n",
                     scanner->indent_stack.size()));
        auto tmp = scanner->scan(lexer, valid_symbols);
        DEBUG(printf("=== Scan end,   stack size = %zu ===\n",
                     scanner->indent_stack.size()));
        return tmp;
    }

    unsigned tree_sitter_miu_external_scanner_serialize(void* payload,
                                                         char* buffer)
    {
        Scanner* scanner = static_cast<Scanner*>(payload);
        DEBUG(printf("=== Serialize start, stack size = %zu ===\n",
                     scanner->indent_stack.size()));
        auto tmp = scanner->serialize(buffer);
        DEBUG(printf("=== Serialize end,   stack size = %zu ===\n",
                     scanner->indent_stack.size()));
        return tmp;
    }

    void tree_sitter_miu_external_scanner_deserialize(void* payload,
                                                      const char* buffer,
                                                      unsigned length)
    {
        Scanner* scanner = static_cast<Scanner*>(payload);
        DEBUG(printf("=== Deserialize start, stack size = %zu ===\n",
                     scanner->indent_stack.size()));
        scanner->deserialize(buffer, length);
        DEBUG(printf("=== Deserialize end,   stack size = %zu ===\n",
                     scanner->indent_stack.size()));
    }

    void tree_sitter_miu_external_scanner_destroy(void* payload) {
        Scanner* scanner = static_cast<Scanner*>(payload);
        DEBUG(printf("=== Delete, stack size = %zu ===\n", scanner->indent_stack.size()));
        delete scanner;
    }

} // end extern "C"
