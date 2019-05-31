#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 9
#define STATE_COUNT 496
#define SYMBOL_COUNT 67
#define ALIAS_COUNT 1
#define TOKEN_COUNT 25
#define EXTERNAL_TOKEN_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 7

enum {
  sym_comment = 1,
  anon_sym_let = 2,
  anon_sym_COLON = 3,
  anon_sym_rec = 4,
  anon_sym_EQ = 5,
  anon_sym_type = 6,
  sym_identifier = 7,
  anon_sym_DOT = 8,
  anon_sym_LPAREN_RPAREN = 9,
  anon_sym_LBRACE = 10,
  anon_sym_COMMA = 11,
  anon_sym_RBRACE = 12,
  anon_sym_LPAREN = 13,
  anon_sym_RPAREN = 14,
  anon_sym_DASH_GT = 15,
  anon_sym_in = 16,
  sym_integer = 17,
  anon_sym_with = 18,
  anon_sym_TILDE = 19,
  anon_sym_where = 20,
  anon_sym_SEMI = 21,
  sym__align = 22,
  sym__indent = 23,
  sym__dedent = 24,
  sym_source_file = 25,
  sym__begin = 26,
  sym__end = 27,
  sym__semi = 28,
  sym_definition = 29,
  sym_top_value_signature = 30,
  sym_top_value_definition = 31,
  sym_top_type_signature = 32,
  sym_top_type_definition = 33,
  sym_parameter_list = 34,
  sym_path = 35,
  sym_type = 36,
  sym_unit_type = 37,
  sym_record_type = 38,
  sym_function_type = 39,
  sym_record_label = 40,
  sym_expression = 41,
  sym_binding = 42,
  sym_pattern = 43,
  sym_let_binding = 44,
  sym_atomic_expression = 45,
  sym_atomic_pattern = 46,
  sym_unit = 47,
  sym_application_expression = 48,
  sym_application_pattern = 49,
  sym_suspension = 50,
  sym_record_expression = 51,
  sym_record_expression_entry = 52,
  sym_record_pattern = 53,
  sym_type_definition_rhs = 54,
  sym_data_type_definition_rhs = 55,
  sym_type_alias_definition_rhs = 56,
  sym_data_constructor_definition = 57,
  aux_sym_source_file_repeat1 = 58,
  aux_sym_parameter_list_repeat1 = 59,
  aux_sym_path_repeat1 = 60,
  aux_sym_record_type_repeat1 = 61,
  aux_sym_application_expression_repeat1 = 62,
  aux_sym_application_pattern_repeat1 = 63,
  aux_sym_record_expression_repeat1 = 64,
  aux_sym_data_type_definition_rhs_repeat1 = 65,
  aux_sym_data_type_definition_rhs_repeat2 = 66,
  anon_alias_sym__semi = 67,
};

static const char *ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_comment] = "comment",
  [anon_sym_let] = "let",
  [anon_sym_COLON] = ":",
  [anon_sym_rec] = "rec",
  [anon_sym_EQ] = "=",
  [anon_sym_type] = "type",
  [sym_identifier] = "identifier",
  [anon_sym_DOT] = ".",
  [anon_sym_LPAREN_RPAREN] = "()",
  [anon_sym_LBRACE] = "{",
  [anon_sym_COMMA] = ",",
  [anon_sym_RBRACE] = "}",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_in] = "in",
  [sym_integer] = "integer",
  [anon_sym_with] = "with",
  [anon_sym_TILDE] = "~",
  [anon_sym_where] = "where",
  [anon_sym_SEMI] = ";",
  [sym__align] = "_align",
  [sym__indent] = "_begin",
  [sym__dedent] = "_end",
  [sym_source_file] = "source_file",
  [sym__begin] = "_begin",
  [sym__end] = "_end",
  [sym__semi] = "_semi",
  [sym_definition] = "definition",
  [sym_top_value_signature] = "top_value_signature",
  [sym_top_value_definition] = "top_value_definition",
  [sym_top_type_signature] = "top_type_signature",
  [sym_top_type_definition] = "top_type_definition",
  [sym_parameter_list] = "parameter_list",
  [sym_path] = "path",
  [sym_type] = "type",
  [sym_unit_type] = "unit_type",
  [sym_record_type] = "record_type",
  [sym_function_type] = "function_type",
  [sym_record_label] = "record_label",
  [sym_expression] = "expression",
  [sym_binding] = "binding",
  [sym_pattern] = "pattern",
  [sym_let_binding] = "let_binding",
  [sym_atomic_expression] = "atomic_expression",
  [sym_atomic_pattern] = "atomic_pattern",
  [sym_unit] = "unit",
  [sym_application_expression] = "application_expression",
  [sym_application_pattern] = "application_pattern",
  [sym_suspension] = "suspension",
  [sym_record_expression] = "record_expression",
  [sym_record_expression_entry] = "record_expression_entry",
  [sym_record_pattern] = "record_pattern",
  [sym_type_definition_rhs] = "type_definition_rhs",
  [sym_data_type_definition_rhs] = "data_type_definition_rhs",
  [sym_type_alias_definition_rhs] = "type_alias_definition_rhs",
  [sym_data_constructor_definition] = "data_constructor_definition",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_parameter_list_repeat1] = "parameter_list_repeat1",
  [aux_sym_path_repeat1] = "path_repeat1",
  [aux_sym_record_type_repeat1] = "record_type_repeat1",
  [aux_sym_application_expression_repeat1] = "application_expression_repeat1",
  [aux_sym_application_pattern_repeat1] = "application_pattern_repeat1",
  [aux_sym_record_expression_repeat1] = "record_expression_repeat1",
  [aux_sym_data_type_definition_rhs_repeat1] = "data_type_definition_rhs_repeat1",
  [aux_sym_data_type_definition_rhs_repeat2] = "data_type_definition_rhs_repeat2",
  [anon_alias_sym__semi] = "_semi",
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_rec] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_in] = {
    .visible = true,
    .named = false,
  },
  [sym_integer] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_with] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_where] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [sym__align] = {
    .visible = false,
    .named = true,
  },
  [sym__indent] = {
    .visible = true,
    .named = false,
  },
  [sym__dedent] = {
    .visible = true,
    .named = false,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__begin] = {
    .visible = false,
    .named = true,
  },
  [sym__end] = {
    .visible = false,
    .named = true,
  },
  [sym__semi] = {
    .visible = false,
    .named = true,
  },
  [sym_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_top_value_signature] = {
    .visible = true,
    .named = true,
  },
  [sym_top_value_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_top_type_signature] = {
    .visible = true,
    .named = true,
  },
  [sym_top_type_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_parameter_list] = {
    .visible = true,
    .named = true,
  },
  [sym_path] = {
    .visible = true,
    .named = true,
  },
  [sym_type] = {
    .visible = true,
    .named = true,
  },
  [sym_unit_type] = {
    .visible = true,
    .named = true,
  },
  [sym_record_type] = {
    .visible = true,
    .named = true,
  },
  [sym_function_type] = {
    .visible = true,
    .named = true,
  },
  [sym_record_label] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_binding] = {
    .visible = true,
    .named = true,
  },
  [sym_pattern] = {
    .visible = true,
    .named = true,
  },
  [sym_let_binding] = {
    .visible = true,
    .named = true,
  },
  [sym_atomic_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_atomic_pattern] = {
    .visible = true,
    .named = true,
  },
  [sym_unit] = {
    .visible = true,
    .named = true,
  },
  [sym_application_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_application_pattern] = {
    .visible = true,
    .named = true,
  },
  [sym_suspension] = {
    .visible = true,
    .named = true,
  },
  [sym_record_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_record_expression_entry] = {
    .visible = true,
    .named = true,
  },
  [sym_record_pattern] = {
    .visible = true,
    .named = true,
  },
  [sym_type_definition_rhs] = {
    .visible = true,
    .named = true,
  },
  [sym_data_type_definition_rhs] = {
    .visible = true,
    .named = true,
  },
  [sym_type_alias_definition_rhs] = {
    .visible = true,
    .named = true,
  },
  [sym_data_constructor_definition] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_parameter_list_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_record_type_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_application_expression_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_application_pattern_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_record_expression_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_data_type_definition_rhs_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_data_type_definition_rhs_repeat2] = {
    .visible = false,
    .named = false,
  },
  [anon_alias_sym__semi] = {
    .visible = true,
    .named = false,
  },
};

static TSSymbol ts_alias_sequences[2][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [1] = {
    [0] = anon_alias_sym__semi,
  },
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  switch (state) {
    case 0:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == ';')
        ADVANCE(8);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(10);
      if (lookahead == 'i')
        ADVANCE(11);
      if (lookahead == 'l')
        ADVANCE(12);
      if (lookahead == 'r')
        ADVANCE(13);
      if (lookahead == 't')
        ADVANCE(14);
      if (lookahead == 'w')
        ADVANCE(15);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '~')
        ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(0);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      END_STATE();
    case 1:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      if (lookahead == ')')
        ADVANCE(20);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 5:
      if (lookahead == '-')
        ADVANCE(21);
      if (lookahead == '>')
        ADVANCE(22);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 10:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(0);
      if (lookahead == '\r')
        SKIP(23);
      END_STATE();
    case 11:
      if (lookahead == 'n')
        ADVANCE(24);
      END_STATE();
    case 12:
      if (lookahead == 'e')
        ADVANCE(25);
      END_STATE();
    case 13:
      if (lookahead == 'e')
        ADVANCE(26);
      END_STATE();
    case 14:
      if (lookahead == 'y')
        ADVANCE(27);
      END_STATE();
    case 15:
      if (lookahead == 'h')
        ADVANCE(28);
      if (lookahead == 'i')
        ADVANCE(29);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      if (lookahead == '-')
        ADVANCE(30);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '_')
        ADVANCE(31);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(31);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_LPAREN_RPAREN);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n')
        ADVANCE(21);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 23:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(0);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_in);
      END_STATE();
    case 25:
      if (lookahead == 't')
        ADVANCE(32);
      END_STATE();
    case 26:
      if (lookahead == 'c')
        ADVANCE(33);
      END_STATE();
    case 27:
      if (lookahead == 'p')
        ADVANCE(34);
      END_STATE();
    case 28:
      if (lookahead == 'e')
        ADVANCE(35);
      END_STATE();
    case 29:
      if (lookahead == 't')
        ADVANCE(36);
      END_STATE();
    case 30:
      if (lookahead == '\n')
        ADVANCE(30);
      if (lookahead == '-')
        ADVANCE(37);
      if (lookahead != 0)
        ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_integer);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_rec);
      END_STATE();
    case 34:
      if (lookahead == 'e')
        ADVANCE(38);
      END_STATE();
    case 35:
      if (lookahead == 'r')
        ADVANCE(39);
      END_STATE();
    case 36:
      if (lookahead == 'h')
        ADVANCE(40);
      END_STATE();
    case 37:
      if (lookahead == '\n')
        ADVANCE(30);
      if (lookahead == '-')
        ADVANCE(37);
      if (lookahead == '}')
        ADVANCE(41);
      if (lookahead != 0)
        ADVANCE(30);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 39:
      if (lookahead == 'e')
        ADVANCE(42);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_with);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\n')
        ADVANCE(30);
      if (lookahead == '-')
        ADVANCE(37);
      if (lookahead != 0)
        ADVANCE(30);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_where);
      END_STATE();
    case 43:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(45);
      if (lookahead == 'l')
        ADVANCE(12);
      if (lookahead == 't')
        ADVANCE(14);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(43);
      END_STATE();
    case 44:
      if (lookahead == '-')
        ADVANCE(21);
      END_STATE();
    case 45:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(43);
      if (lookahead == '\r')
        SKIP(47);
      END_STATE();
    case 46:
      if (lookahead == '-')
        ADVANCE(30);
      END_STATE();
    case 47:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(43);
      END_STATE();
    case 48:
      if (lookahead == '(')
        ADVANCE(49);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(50);
      if (lookahead == 'r')
        ADVANCE(51);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(48);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 49:
      if (lookahead == ')')
        ADVANCE(20);
      END_STATE();
    case 50:
      if (lookahead == '\n')
        SKIP(48);
      if (lookahead == '\r')
        SKIP(53);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 53:
      if (lookahead == '\n')
        SKIP(48);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c')
        ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_rec);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 56:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(57);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(56);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 57:
      if (lookahead == '\n')
        SKIP(56);
      if (lookahead == '\r')
        SKIP(58);
      END_STATE();
    case 58:
      if (lookahead == '\n')
        SKIP(56);
      END_STATE();
    case 59:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(60);
      if (lookahead == 'w')
        ADVANCE(61);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '~')
        ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(59);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 60:
      if (lookahead == '\n')
        SKIP(59);
      if (lookahead == '\r')
        SKIP(62);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i')
        ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 62:
      if (lookahead == '\n')
        SKIP(59);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't')
        ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h')
        ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_with);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 66:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(67);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(66);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 67:
      if (lookahead == '\n')
        SKIP(66);
      if (lookahead == '\r')
        SKIP(68);
      END_STATE();
    case 68:
      if (lookahead == '\n')
        SKIP(66);
      END_STATE();
    case 69:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(70);
      if (lookahead == 'w')
        ADVANCE(71);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(69);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 70:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(69);
      if (lookahead == '\r')
        SKIP(72);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h')
        ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 72:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(69);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(74);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r')
        ADVANCE(75);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(76);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_where);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 77:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(78);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '~')
        ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(77);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 78:
      if (lookahead == '\n')
        SKIP(77);
      if (lookahead == '\r')
        SKIP(79);
      END_STATE();
    case 79:
      if (lookahead == '\n')
        SKIP(77);
      END_STATE();
    case 80:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(81);
      if (lookahead == 'w')
        ADVANCE(82);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(80);
      END_STATE();
    case 81:
      if (lookahead == '\n')
        SKIP(80);
      if (lookahead == '\r')
        SKIP(83);
      END_STATE();
    case 82:
      if (lookahead == 'i')
        ADVANCE(29);
      END_STATE();
    case 83:
      if (lookahead == '\n')
        SKIP(80);
      END_STATE();
    case 84:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(85);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(84);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 85:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(84);
      if (lookahead == '\r')
        SKIP(87);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(88);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 87:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(84);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't')
        ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 90:
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == ';')
        ADVANCE(8);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(91);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(90);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 91:
      if (lookahead == '\n')
        SKIP(90);
      if (lookahead == '\r')
        SKIP(92);
      END_STATE();
    case 92:
      if (lookahead == '\n')
        SKIP(90);
      END_STATE();
    case 93:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(94);
      if (lookahead == 'w')
        ADVANCE(95);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(93);
      END_STATE();
    case 94:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(93);
      if (lookahead == '\r')
        SKIP(96);
      END_STATE();
    case 95:
      if (lookahead == 'h')
        ADVANCE(28);
      END_STATE();
    case 96:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(93);
      END_STATE();
    case 97:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(98);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == 'w')
        ADVANCE(61);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '~')
        ADVANCE(18);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(97);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 98:
      if (lookahead == '\n')
        SKIP(97);
      if (lookahead == '\r')
        SKIP(99);
      END_STATE();
    case 99:
      if (lookahead == '\n')
        SKIP(97);
      END_STATE();
    case 100:
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(101);
      if (lookahead == 'w')
        ADVANCE(15);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(100);
      END_STATE();
    case 101:
      if (lookahead == '\n')
        SKIP(100);
      if (lookahead == '\r')
        SKIP(102);
      END_STATE();
    case 102:
      if (lookahead == '\n')
        SKIP(100);
      END_STATE();
    case 103:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(104);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(103);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 104:
      if (lookahead == '\n')
        SKIP(103);
      if (lookahead == '\r')
        SKIP(105);
      END_STATE();
    case 105:
      if (lookahead == '\n')
        SKIP(103);
      END_STATE();
    case 106:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(107);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == 'w')
        ADVANCE(61);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(106);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 107:
      if (lookahead == '\n')
        SKIP(106);
      if (lookahead == '\r')
        SKIP(108);
      END_STATE();
    case 108:
      if (lookahead == '\n')
        SKIP(106);
      END_STATE();
    case 109:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(110);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(109);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 110:
      if (lookahead == '\n')
        SKIP(109);
      if (lookahead == '\r')
        SKIP(111);
      END_STATE();
    case 111:
      if (lookahead == '\n')
        SKIP(109);
      END_STATE();
    case 112:
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ';')
        ADVANCE(8);
      if (lookahead == '\\')
        SKIP(113);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(112);
      END_STATE();
    case 113:
      if (lookahead == '\n')
        SKIP(112);
      if (lookahead == '\r')
        SKIP(114);
      END_STATE();
    case 114:
      if (lookahead == '\n')
        SKIP(112);
      END_STATE();
    case 115:
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == ';')
        ADVANCE(8);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(116);
      if (lookahead == 'w')
        ADVANCE(95);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(115);
      END_STATE();
    case 116:
      if (lookahead == '\n')
        SKIP(115);
      if (lookahead == '\r')
        SKIP(117);
      END_STATE();
    case 117:
      if (lookahead == '\n')
        SKIP(115);
      END_STATE();
    case 118:
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(119);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '}')
        ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(118);
      END_STATE();
    case 119:
      if (lookahead == '\n')
        SKIP(118);
      if (lookahead == '\r')
        SKIP(120);
      END_STATE();
    case 120:
      if (lookahead == '\n')
        SKIP(118);
      END_STATE();
    case 121:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(122);
      if (lookahead == 'i')
        ADVANCE(123);
      if (lookahead == 'l')
        ADVANCE(86);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(121);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 122:
      if (lookahead == '\n')
        SKIP(121);
      if (lookahead == '\r')
        SKIP(124);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n')
        ADVANCE(125);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 124:
      if (lookahead == '\n')
        SKIP(121);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(anon_sym_in);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    default:
      return false;
  }
}

static TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0, .external_lex_state = 1},
  [1] = {.lex_state = 43, .external_lex_state = 2},
  [2] = {.lex_state = 43},
  [3] = {.lex_state = 48},
  [4] = {.lex_state = 56},
  [5] = {.lex_state = 0, .external_lex_state = 2},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 43},
  [8] = {.lex_state = 0, .external_lex_state = 2},
  [9] = {.lex_state = 43},
  [10] = {.lex_state = 0, .external_lex_state = 2},
  [11] = {.lex_state = 56},
  [12] = {.lex_state = 56},
  [13] = {.lex_state = 56},
  [14] = {.lex_state = 56},
  [15] = {.lex_state = 59},
  [16] = {.lex_state = 66},
  [17] = {.lex_state = 56},
  [18] = {.lex_state = 56},
  [19] = {.lex_state = 56},
  [20] = {.lex_state = 56},
  [21] = {.lex_state = 56},
  [22] = {.lex_state = 56},
  [23] = {.lex_state = 69},
  [24] = {.lex_state = 43},
  [25] = {.lex_state = 43},
  [26] = {.lex_state = 0, .external_lex_state = 2},
  [27] = {.lex_state = 56},
  [28] = {.lex_state = 56},
  [29] = {.lex_state = 56},
  [30] = {.lex_state = 56},
  [31] = {.lex_state = 56},
  [32] = {.lex_state = 56},
  [33] = {.lex_state = 77},
  [34] = {.lex_state = 80},
  [35] = {.lex_state = 80},
  [36] = {.lex_state = 84},
  [37] = {.lex_state = 77},
  [38] = {.lex_state = 56},
  [39] = {.lex_state = 56},
  [40] = {.lex_state = 56},
  [41] = {.lex_state = 84},
  [42] = {.lex_state = 56},
  [43] = {.lex_state = 56},
  [44] = {.lex_state = 56},
  [45] = {.lex_state = 48, .external_lex_state = 3},
  [46] = {.lex_state = 56},
  [47] = {.lex_state = 56},
  [48] = {.lex_state = 0, .external_lex_state = 2},
  [49] = {.lex_state = 69},
  [50] = {.lex_state = 69},
  [51] = {.lex_state = 0, .external_lex_state = 2},
  [52] = {.lex_state = 48},
  [53] = {.lex_state = 56},
  [54] = {.lex_state = 0, .external_lex_state = 2},
  [55] = {.lex_state = 84},
  [56] = {.lex_state = 56},
  [57] = {.lex_state = 56},
  [58] = {.lex_state = 84},
  [59] = {.lex_state = 80},
  [60] = {.lex_state = 56},
  [61] = {.lex_state = 56},
  [62] = {.lex_state = 84},
  [63] = {.lex_state = 77},
  [64] = {.lex_state = 56},
  [65] = {.lex_state = 84},
  [66] = {.lex_state = 80},
  [67] = {.lex_state = 77},
  [68] = {.lex_state = 77},
  [69] = {.lex_state = 77},
  [70] = {.lex_state = 90},
  [71] = {.lex_state = 69, .external_lex_state = 2},
  [72] = {.lex_state = 56},
  [73] = {.lex_state = 56},
  [74] = {.lex_state = 56},
  [75] = {.lex_state = 93, .external_lex_state = 2},
  [76] = {.lex_state = 69, .external_lex_state = 2},
  [77] = {.lex_state = 93, .external_lex_state = 2},
  [78] = {.lex_state = 0, .external_lex_state = 2},
  [79] = {.lex_state = 56},
  [80] = {.lex_state = 97},
  [81] = {.lex_state = 84, .external_lex_state = 2},
  [82] = {.lex_state = 84, .external_lex_state = 2},
  [83] = {.lex_state = 84},
  [84] = {.lex_state = 56},
  [85] = {.lex_state = 84, .external_lex_state = 2},
  [86] = {.lex_state = 84, .external_lex_state = 2},
  [87] = {.lex_state = 56},
  [88] = {.lex_state = 56},
  [89] = {.lex_state = 56, .external_lex_state = 4},
  [90] = {.lex_state = 56},
  [91] = {.lex_state = 56},
  [92] = {.lex_state = 93, .external_lex_state = 2},
  [93] = {.lex_state = 93, .external_lex_state = 2},
  [94] = {.lex_state = 69, .external_lex_state = 2},
  [95] = {.lex_state = 0, .external_lex_state = 2},
  [96] = {.lex_state = 69},
  [97] = {.lex_state = 56},
  [98] = {.lex_state = 0, .external_lex_state = 2},
  [99] = {.lex_state = 56},
  [100] = {.lex_state = 66},
  [101] = {.lex_state = 56},
  [102] = {.lex_state = 69},
  [103] = {.lex_state = 84, .external_lex_state = 2},
  [104] = {.lex_state = 84},
  [105] = {.lex_state = 80},
  [106] = {.lex_state = 56},
  [107] = {.lex_state = 84},
  [108] = {.lex_state = 100},
  [109] = {.lex_state = 97},
  [110] = {.lex_state = 103},
  [111] = {.lex_state = 103},
  [112] = {.lex_state = 84},
  [113] = {.lex_state = 56},
  [114] = {.lex_state = 103},
  [115] = {.lex_state = 84},
  [116] = {.lex_state = 80},
  [117] = {.lex_state = 77},
  [118] = {.lex_state = 56},
  [119] = {.lex_state = 84},
  [120] = {.lex_state = 93, .external_lex_state = 2},
  [121] = {.lex_state = 56},
  [122] = {.lex_state = 66},
  [123] = {.lex_state = 66},
  [124] = {.lex_state = 69, .external_lex_state = 2},
  [125] = {.lex_state = 66},
  [126] = {.lex_state = 66},
  [127] = {.lex_state = 56},
  [128] = {.lex_state = 56},
  [129] = {.lex_state = 93, .external_lex_state = 2},
  [130] = {.lex_state = 56},
  [131] = {.lex_state = 77},
  [132] = {.lex_state = 84, .external_lex_state = 2},
  [133] = {.lex_state = 56},
  [134] = {.lex_state = 106},
  [135] = {.lex_state = 84},
  [136] = {.lex_state = 84},
  [137] = {.lex_state = 80},
  [138] = {.lex_state = 77},
  [139] = {.lex_state = 56},
  [140] = {.lex_state = 109},
  [141] = {.lex_state = 56},
  [142] = {.lex_state = 56},
  [143] = {.lex_state = 84, .external_lex_state = 2},
  [144] = {.lex_state = 84, .external_lex_state = 2},
  [145] = {.lex_state = 0, .external_lex_state = 2},
  [146] = {.lex_state = 66},
  [147] = {.lex_state = 56},
  [148] = {.lex_state = 112},
  [149] = {.lex_state = 66},
  [150] = {.lex_state = 56, .external_lex_state = 4},
  [151] = {.lex_state = 0, .external_lex_state = 5},
  [152] = {.lex_state = 93, .external_lex_state = 2},
  [153] = {.lex_state = 66},
  [154] = {.lex_state = 93, .external_lex_state = 2},
  [155] = {.lex_state = 56},
  [156] = {.lex_state = 0, .external_lex_state = 2},
  [157] = {.lex_state = 115},
  [158] = {.lex_state = 56},
  [159] = {.lex_state = 56},
  [160] = {.lex_state = 56},
  [161] = {.lex_state = 93},
  [162] = {.lex_state = 115},
  [163] = {.lex_state = 93},
  [164] = {.lex_state = 69},
  [165] = {.lex_state = 56},
  [166] = {.lex_state = 56},
  [167] = {.lex_state = 84},
  [168] = {.lex_state = 56},
  [169] = {.lex_state = 56},
  [170] = {.lex_state = 56},
  [171] = {.lex_state = 69},
  [172] = {.lex_state = 84, .external_lex_state = 2},
  [173] = {.lex_state = 56},
  [174] = {.lex_state = 77},
  [175] = {.lex_state = 103},
  [176] = {.lex_state = 84},
  [177] = {.lex_state = 84},
  [178] = {.lex_state = 80},
  [179] = {.lex_state = 77},
  [180] = {.lex_state = 109},
  [181] = {.lex_state = 56},
  [182] = {.lex_state = 84},
  [183] = {.lex_state = 84},
  [184] = {.lex_state = 84},
  [185] = {.lex_state = 84},
  [186] = {.lex_state = 93, .external_lex_state = 2},
  [187] = {.lex_state = 66},
  [188] = {.lex_state = 66},
  [189] = {.lex_state = 56},
  [190] = {.lex_state = 56},
  [191] = {.lex_state = 69, .external_lex_state = 2},
  [192] = {.lex_state = 66},
  [193] = {.lex_state = 56},
  [194] = {.lex_state = 93, .external_lex_state = 2},
  [195] = {.lex_state = 93, .external_lex_state = 2},
  [196] = {.lex_state = 69, .external_lex_state = 2},
  [197] = {.lex_state = 84, .external_lex_state = 2},
  [198] = {.lex_state = 84},
  [199] = {.lex_state = 77},
  [200] = {.lex_state = 56},
  [201] = {.lex_state = 84, .external_lex_state = 2},
  [202] = {.lex_state = 84},
  [203] = {.lex_state = 84},
  [204] = {.lex_state = 77},
  [205] = {.lex_state = 56},
  [206] = {.lex_state = 84, .external_lex_state = 2},
  [207] = {.lex_state = 109},
  [208] = {.lex_state = 109},
  [209] = {.lex_state = 84},
  [210] = {.lex_state = 84, .external_lex_state = 2},
  [211] = {.lex_state = 56},
  [212] = {.lex_state = 0, .external_lex_state = 2},
  [213] = {.lex_state = 56},
  [214] = {.lex_state = 112},
  [215] = {.lex_state = 56},
  [216] = {.lex_state = 56},
  [217] = {.lex_state = 56, .external_lex_state = 4},
  [218] = {.lex_state = 0, .external_lex_state = 5},
  [219] = {.lex_state = 56, .external_lex_state = 4},
  [220] = {.lex_state = 56, .external_lex_state = 4},
  [221] = {.lex_state = 93, .external_lex_state = 2},
  [222] = {.lex_state = 56},
  [223] = {.lex_state = 93, .external_lex_state = 2},
  [224] = {.lex_state = 93},
  [225] = {.lex_state = 66},
  [226] = {.lex_state = 115},
  [227] = {.lex_state = 66},
  [228] = {.lex_state = 56},
  [229] = {.lex_state = 93},
  [230] = {.lex_state = 56},
  [231] = {.lex_state = 0, .external_lex_state = 2},
  [232] = {.lex_state = 84},
  [233] = {.lex_state = 56},
  [234] = {.lex_state = 56},
  [235] = {.lex_state = 56},
  [236] = {.lex_state = 93, .external_lex_state = 2},
  [237] = {.lex_state = 93, .external_lex_state = 2},
  [238] = {.lex_state = 56},
  [239] = {.lex_state = 84, .external_lex_state = 2},
  [240] = {.lex_state = 56},
  [241] = {.lex_state = 56},
  [242] = {.lex_state = 93, .external_lex_state = 2},
  [243] = {.lex_state = 93, .external_lex_state = 2},
  [244] = {.lex_state = 69, .external_lex_state = 2},
  [245] = {.lex_state = 56},
  [246] = {.lex_state = 103},
  [247] = {.lex_state = 84},
  [248] = {.lex_state = 77},
  [249] = {.lex_state = 103},
  [250] = {.lex_state = 77},
  [251] = {.lex_state = 103},
  [252] = {.lex_state = 84},
  [253] = {.lex_state = 84},
  [254] = {.lex_state = 56},
  [255] = {.lex_state = 56},
  [256] = {.lex_state = 84},
  [257] = {.lex_state = 66},
  [258] = {.lex_state = 66},
  [259] = {.lex_state = 56},
  [260] = {.lex_state = 109},
  [261] = {.lex_state = 109},
  [262] = {.lex_state = 56},
  [263] = {.lex_state = 56},
  [264] = {.lex_state = 118},
  [265] = {.lex_state = 118},
  [266] = {.lex_state = 84},
  [267] = {.lex_state = 56},
  [268] = {.lex_state = 66},
  [269] = {.lex_state = 84, .external_lex_state = 2},
  [270] = {.lex_state = 84},
  [271] = {.lex_state = 84},
  [272] = {.lex_state = 84},
  [273] = {.lex_state = 77},
  [274] = {.lex_state = 84},
  [275] = {.lex_state = 109},
  [276] = {.lex_state = 97},
  [277] = {.lex_state = 121},
  [278] = {.lex_state = 121},
  [279] = {.lex_state = 84},
  [280] = {.lex_state = 56},
  [281] = {.lex_state = 121},
  [282] = {.lex_state = 121},
  [283] = {.lex_state = 56},
  [284] = {.lex_state = 56},
  [285] = {.lex_state = 112},
  [286] = {.lex_state = 112},
  [287] = {.lex_state = 112},
  [288] = {.lex_state = 66},
  [289] = {.lex_state = 112},
  [290] = {.lex_state = 0, .external_lex_state = 2},
  [291] = {.lex_state = 0, .external_lex_state = 5},
  [292] = {.lex_state = 56},
  [293] = {.lex_state = 56},
  [294] = {.lex_state = 56},
  [295] = {.lex_state = 93, .external_lex_state = 5},
  [296] = {.lex_state = 0, .external_lex_state = 5},
  [297] = {.lex_state = 93, .external_lex_state = 5},
  [298] = {.lex_state = 0, .external_lex_state = 5},
  [299] = {.lex_state = 66},
  [300] = {.lex_state = 0, .external_lex_state = 2},
  [301] = {.lex_state = 109},
  [302] = {.lex_state = 93},
  [303] = {.lex_state = 56},
  [304] = {.lex_state = 56},
  [305] = {.lex_state = 115},
  [306] = {.lex_state = 66},
  [307] = {.lex_state = 93},
  [308] = {.lex_state = 115},
  [309] = {.lex_state = 84, .external_lex_state = 2},
  [310] = {.lex_state = 84},
  [311] = {.lex_state = 93, .external_lex_state = 2},
  [312] = {.lex_state = 66},
  [313] = {.lex_state = 93, .external_lex_state = 2},
  [314] = {.lex_state = 56},
  [315] = {.lex_state = 56},
  [316] = {.lex_state = 84, .external_lex_state = 2},
  [317] = {.lex_state = 84, .external_lex_state = 2},
  [318] = {.lex_state = 93, .external_lex_state = 2},
  [319] = {.lex_state = 66},
  [320] = {.lex_state = 93, .external_lex_state = 2},
  [321] = {.lex_state = 56},
  [322] = {.lex_state = 69},
  [323] = {.lex_state = 103},
  [324] = {.lex_state = 84},
  [325] = {.lex_state = 77},
  [326] = {.lex_state = 121},
  [327] = {.lex_state = 56},
  [328] = {.lex_state = 84},
  [329] = {.lex_state = 84},
  [330] = {.lex_state = 109},
  [331] = {.lex_state = 109},
  [332] = {.lex_state = 93},
  [333] = {.lex_state = 118},
  [334] = {.lex_state = 66},
  [335] = {.lex_state = 118},
  [336] = {.lex_state = 56},
  [337] = {.lex_state = 69, .external_lex_state = 2},
  [338] = {.lex_state = 56},
  [339] = {.lex_state = 84},
  [340] = {.lex_state = 56},
  [341] = {.lex_state = 84, .external_lex_state = 2},
  [342] = {.lex_state = 121},
  [343] = {.lex_state = 84},
  [344] = {.lex_state = 121},
  [345] = {.lex_state = 77},
  [346] = {.lex_state = 121},
  [347] = {.lex_state = 84},
  [348] = {.lex_state = 84},
  [349] = {.lex_state = 80},
  [350] = {.lex_state = 77},
  [351] = {.lex_state = 109},
  [352] = {.lex_state = 56},
  [353] = {.lex_state = 84},
  [354] = {.lex_state = 121},
  [355] = {.lex_state = 121},
  [356] = {.lex_state = 112},
  [357] = {.lex_state = 66},
  [358] = {.lex_state = 112},
  [359] = {.lex_state = 56},
  [360] = {.lex_state = 56},
  [361] = {.lex_state = 93, .external_lex_state = 5},
  [362] = {.lex_state = 66},
  [363] = {.lex_state = 0, .external_lex_state = 5},
  [364] = {.lex_state = 66},
  [365] = {.lex_state = 56},
  [366] = {.lex_state = 56},
  [367] = {.lex_state = 93, .external_lex_state = 5},
  [368] = {.lex_state = 56},
  [369] = {.lex_state = 56},
  [370] = {.lex_state = 93},
  [371] = {.lex_state = 109},
  [372] = {.lex_state = 84},
  [373] = {.lex_state = 56},
  [374] = {.lex_state = 84, .external_lex_state = 2},
  [375] = {.lex_state = 93, .external_lex_state = 2},
  [376] = {.lex_state = 56},
  [377] = {.lex_state = 93, .external_lex_state = 2},
  [378] = {.lex_state = 84},
  [379] = {.lex_state = 84, .external_lex_state = 2},
  [380] = {.lex_state = 93, .external_lex_state = 2},
  [381] = {.lex_state = 56},
  [382] = {.lex_state = 93, .external_lex_state = 2},
  [383] = {.lex_state = 103},
  [384] = {.lex_state = 84},
  [385] = {.lex_state = 84},
  [386] = {.lex_state = 84},
  [387] = {.lex_state = 84},
  [388] = {.lex_state = 109},
  [389] = {.lex_state = 109},
  [390] = {.lex_state = 56},
  [391] = {.lex_state = 118},
  [392] = {.lex_state = 56},
  [393] = {.lex_state = 118},
  [394] = {.lex_state = 69, .external_lex_state = 2},
  [395] = {.lex_state = 56},
  [396] = {.lex_state = 56},
  [397] = {.lex_state = 118},
  [398] = {.lex_state = 118},
  [399] = {.lex_state = 84},
  [400] = {.lex_state = 84},
  [401] = {.lex_state = 84, .external_lex_state = 2},
  [402] = {.lex_state = 84},
  [403] = {.lex_state = 121},
  [404] = {.lex_state = 84},
  [405] = {.lex_state = 77},
  [406] = {.lex_state = 121},
  [407] = {.lex_state = 77},
  [408] = {.lex_state = 121},
  [409] = {.lex_state = 84},
  [410] = {.lex_state = 84, .external_lex_state = 2},
  [411] = {.lex_state = 121},
  [412] = {.lex_state = 112},
  [413] = {.lex_state = 56},
  [414] = {.lex_state = 112},
  [415] = {.lex_state = 56},
  [416] = {.lex_state = 56},
  [417] = {.lex_state = 112},
  [418] = {.lex_state = 112},
  [419] = {.lex_state = 93, .external_lex_state = 5},
  [420] = {.lex_state = 56},
  [421] = {.lex_state = 56},
  [422] = {.lex_state = 0, .external_lex_state = 5},
  [423] = {.lex_state = 66},
  [424] = {.lex_state = 93, .external_lex_state = 5},
  [425] = {.lex_state = 93, .external_lex_state = 5},
  [426] = {.lex_state = 0, .external_lex_state = 5},
  [427] = {.lex_state = 0, .external_lex_state = 2},
  [428] = {.lex_state = 56},
  [429] = {.lex_state = 93},
  [430] = {.lex_state = 115},
  [431] = {.lex_state = 84},
  [432] = {.lex_state = 109},
  [433] = {.lex_state = 121},
  [434] = {.lex_state = 109},
  [435] = {.lex_state = 103},
  [436] = {.lex_state = 84},
  [437] = {.lex_state = 121},
  [438] = {.lex_state = 69, .external_lex_state = 2},
  [439] = {.lex_state = 109},
  [440] = {.lex_state = 118},
  [441] = {.lex_state = 66},
  [442] = {.lex_state = 118},
  [443] = {.lex_state = 56},
  [444] = {.lex_state = 84},
  [445] = {.lex_state = 109},
  [446] = {.lex_state = 121},
  [447] = {.lex_state = 84},
  [448] = {.lex_state = 77},
  [449] = {.lex_state = 121},
  [450] = {.lex_state = 109},
  [451] = {.lex_state = 112},
  [452] = {.lex_state = 66},
  [453] = {.lex_state = 112},
  [454] = {.lex_state = 56},
  [455] = {.lex_state = 109},
  [456] = {.lex_state = 84},
  [457] = {.lex_state = 56},
  [458] = {.lex_state = 56},
  [459] = {.lex_state = 115},
  [460] = {.lex_state = 93},
  [461] = {.lex_state = 84},
  [462] = {.lex_state = 93},
  [463] = {.lex_state = 84},
  [464] = {.lex_state = 93},
  [465] = {.lex_state = 118},
  [466] = {.lex_state = 56},
  [467] = {.lex_state = 118},
  [468] = {.lex_state = 121},
  [469] = {.lex_state = 84},
  [470] = {.lex_state = 84},
  [471] = {.lex_state = 93},
  [472] = {.lex_state = 112},
  [473] = {.lex_state = 56},
  [474] = {.lex_state = 112},
  [475] = {.lex_state = 93},
  [476] = {.lex_state = 0, .external_lex_state = 5},
  [477] = {.lex_state = 84},
  [478] = {.lex_state = 115},
  [479] = {.lex_state = 56},
  [480] = {.lex_state = 84, .external_lex_state = 2},
  [481] = {.lex_state = 56},
  [482] = {.lex_state = 84},
  [483] = {.lex_state = 56},
  [484] = {.lex_state = 109},
  [485] = {.lex_state = 121},
  [486] = {.lex_state = 121},
  [487] = {.lex_state = 56},
  [488] = {.lex_state = 109},
  [489] = {.lex_state = 56},
  [490] = {.lex_state = 0, .external_lex_state = 5},
  [491] = {.lex_state = 93},
  [492] = {.lex_state = 93},
  [493] = {.lex_state = 0, .external_lex_state = 5},
  [494] = {.lex_state = 56},
  [495] = {.lex_state = 56},
};

enum {
  ts_external_token__align = 0,
  ts_external_token__indent = 1,
  ts_external_token__dedent = 2,
};

static TSSymbol ts_external_scanner_symbol_map[EXTERNAL_TOKEN_COUNT] = {
  [ts_external_token__align] = sym__align,
  [ts_external_token__indent] = sym__indent,
  [ts_external_token__dedent] = sym__dedent,
};

static bool ts_external_scanner_states[6][EXTERNAL_TOKEN_COUNT] = {
  [1] = {
    [ts_external_token__align] = true,
    [ts_external_token__indent] = true,
    [ts_external_token__dedent] = true,
  },
  [2] = {
    [ts_external_token__align] = true,
  },
  [3] = {
    [ts_external_token__indent] = true,
  },
  [4] = {
    [ts_external_token__dedent] = true,
  },
  [5] = {
    [ts_external_token__align] = true,
    [ts_external_token__dedent] = true,
  },
};

static uint16_t ts_parse_table[STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [anon_sym_where] = ACTIONS(1),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(1),
    [sym__align] = ACTIONS(1),
    [sym_integer] = ACTIONS(1),
    [sym__dedent] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_rec] = ACTIONS(1),
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_in] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [sym__indent] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_with] = ACTIONS(1),
  },
  [1] = {
    [sym_top_value_signature] = STATE(5),
    [sym_definition] = STATE(8),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(7),
    [sym_top_value_definition] = STATE(5),
    [sym_source_file] = STATE(6),
    [sym_top_type_signature] = STATE(5),
    [sym__align] = ACTIONS(5),
    [ts_builtin_sym_end] = ACTIONS(7),
    [anon_sym_type] = ACTIONS(9),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(11),
  },
  [2] = {
    [sym_top_value_signature] = STATE(5),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(9),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(10),
    [anon_sym_type] = ACTIONS(9),
    [ts_builtin_sym_end] = ACTIONS(13),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(11),
  },
  [3] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(22),
    [sym_binding] = STATE(21),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(22),
    [sym_record_pattern] = STATE(18),
    [anon_sym_rec] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(25),
  },
  [4] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(27),
  },
  [5] = {
    [sym__align] = ACTIONS(29),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(29),
  },
  [6] = {
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(31),
  },
  [7] = {
    [sym_top_value_signature] = STATE(5),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(24),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(10),
    [anon_sym_type] = ACTIONS(9),
    [ts_builtin_sym_end] = ACTIONS(13),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(11),
  },
  [8] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(13),
  },
  [9] = {
    [sym_top_value_signature] = STATE(5),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(24),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(26),
    [anon_sym_type] = ACTIONS(9),
    [ts_builtin_sym_end] = ACTIONS(35),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(11),
  },
  [10] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(35),
  },
  [11] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(22),
    [sym_binding] = STATE(28),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(22),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [12] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(39),
    [sym_identifier] = ACTIONS(39),
  },
  [13] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(41),
    [sym_identifier] = ACTIONS(41),
  },
  [14] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(43),
  },
  [15] = {
    [sym_path] = STATE(35),
    [sym_record_expression_entry] = STATE(36),
    [aux_sym_record_expression_repeat1] = STATE(37),
    [anon_sym_RBRACE] = ACTIONS(45),
    [anon_sym_TILDE] = ACTIONS(47),
    [anon_sym_DOT] = ACTIONS(49),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(51),
    [sym_identifier] = ACTIONS(53),
  },
  [16] = {
    [aux_sym_path_repeat1] = STATE(40),
    [anon_sym_COLON] = ACTIONS(55),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [17] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(59),
    [sym_identifier] = ACTIONS(59),
  },
  [18] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(61),
    [sym_identifier] = ACTIONS(61),
  },
  [19] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(63),
    [sym_identifier] = ACTIONS(63),
  },
  [20] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(65),
    [sym_identifier] = ACTIONS(65),
  },
  [21] = {
    [sym_parameter_list] = STATE(42),
    [aux_sym_parameter_list_repeat1] = STATE(43),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(67),
    [sym_identifier] = ACTIONS(69),
  },
  [22] = {
    [sym_path] = STATE(44),
    [aux_sym_application_pattern_repeat1] = STATE(44),
    [anon_sym_DOT] = ACTIONS(21),
    [anon_sym_EQ] = ACTIONS(71),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(71),
  },
  [23] = {
    [sym_type_definition_rhs] = STATE(48),
    [aux_sym_parameter_list_repeat1] = STATE(49),
    [sym_parameter_list] = STATE(50),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(73),
    [anon_sym_COLON] = ACTIONS(75),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(77),
    [sym_identifier] = ACTIONS(79),
  },
  [24] = {
    [sym_top_value_signature] = STATE(5),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(24),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(54),
    [ts_builtin_sym_end] = ACTIONS(81),
    [anon_sym_type] = ACTIONS(83),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(86),
  },
  [25] = {
    [ts_builtin_sym_end] = ACTIONS(81),
    [anon_sym_type] = ACTIONS(81),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(81),
  },
  [26] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(89),
  },
  [27] = {
    [aux_sym_path_repeat1] = STATE(40),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [28] = {
    [sym_parameter_list] = STATE(56),
    [aux_sym_parameter_list_repeat1] = STATE(43),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(91),
    [sym_identifier] = ACTIONS(69),
  },
  [29] = {
    [aux_sym_path_repeat1] = STATE(57),
    [anon_sym_DOT] = ACTIONS(93),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(93),
  },
  [30] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_identifier] = ACTIONS(95),
  },
  [31] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(97),
  },
  [32] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(99),
  },
  [33] = {
    [sym_record_expression_entry] = STATE(62),
    [aux_sym_record_expression_repeat1] = STATE(63),
    [anon_sym_RBRACE] = ACTIONS(101),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [34] = {
    [aux_sym_path_repeat1] = STATE(66),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_EQ] = ACTIONS(107),
    [anon_sym_with] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
  },
  [35] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(109),
  },
  [36] = {
    [anon_sym_RBRACE] = ACTIONS(101),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [37] = {
    [sym_record_expression_entry] = STATE(62),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(101),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [38] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(113),
  },
  [39] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(77),
    [sym_type] = STATE(78),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(117),
    [anon_sym_LPAREN] = ACTIONS(119),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(123),
  },
  [40] = {
    [aux_sym_path_repeat1] = STATE(79),
    [anon_sym_DOT] = ACTIONS(93),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(93),
  },
  [41] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(86),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(135),
  },
  [42] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(91),
  },
  [43] = {
    [aux_sym_parameter_list_repeat1] = STATE(87),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(137),
    [sym_identifier] = ACTIONS(139),
  },
  [44] = {
    [sym_path] = STATE(44),
    [aux_sym_application_pattern_repeat1] = STATE(44),
    [anon_sym_DOT] = ACTIONS(141),
    [anon_sym_EQ] = ACTIONS(144),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(146),
  },
  [45] = {
    [sym__begin] = STATE(89),
    [sym_comment] = ACTIONS(3),
    [sym__indent] = ACTIONS(149),
    [anon_sym_LBRACE] = ACTIONS(151),
  },
  [46] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(93),
    [sym_type] = STATE(94),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(153),
    [anon_sym_LPAREN] = ACTIONS(155),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(157),
  },
  [47] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(77),
    [sym_type] = STATE(95),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(117),
    [anon_sym_LPAREN] = ACTIONS(119),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(123),
  },
  [48] = {
    [sym__align] = ACTIONS(159),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(159),
  },
  [49] = {
    [aux_sym_parameter_list_repeat1] = STATE(96),
    [anon_sym_where] = ACTIONS(161),
    [anon_sym_COLON] = ACTIONS(137),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(137),
    [sym_identifier] = ACTIONS(163),
  },
  [50] = {
    [sym_type_definition_rhs] = STATE(98),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(165),
    [anon_sym_COLON] = ACTIONS(167),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(77),
  },
  [51] = {
    [sym__align] = ACTIONS(169),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(169),
  },
  [52] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(22),
    [sym_binding] = STATE(101),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(22),
    [sym_record_pattern] = STATE(18),
    [anon_sym_rec] = ACTIONS(171),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(173),
  },
  [53] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(175),
  },
  [54] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
  },
  [55] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(103),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(135),
  },
  [56] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(177),
  },
  [57] = {
    [aux_sym_path_repeat1] = STATE(79),
    [anon_sym_DOT] = ACTIONS(179),
    [anon_sym_EQ] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(179),
  },
  [58] = {
    [anon_sym_RBRACE] = ACTIONS(181),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(181),
  },
  [59] = {
    [aux_sym_path_repeat1] = STATE(105),
    [anon_sym_DOT] = ACTIONS(105),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(93),
  },
  [60] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(107),
  },
  [61] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(183),
    [sym_identifier] = ACTIONS(183),
  },
  [62] = {
    [anon_sym_RBRACE] = ACTIONS(185),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [63] = {
    [sym_record_expression_entry] = STATE(107),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(185),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [64] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(187),
  },
  [65] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(115),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(199),
  },
  [66] = {
    [aux_sym_path_repeat1] = STATE(116),
    [anon_sym_DOT] = ACTIONS(105),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(93),
  },
  [67] = {
    [sym_record_expression_entry] = STATE(107),
    [aux_sym_record_expression_repeat1] = STATE(117),
    [anon_sym_RBRACE] = ACTIONS(185),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [68] = {
    [anon_sym_RBRACE] = ACTIONS(201),
    [anon_sym_TILDE] = ACTIONS(201),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(201),
  },
  [69] = {
    [sym_record_expression_entry] = STATE(119),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(201),
    [anon_sym_TILDE] = ACTIONS(203),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(206),
  },
  [70] = {
    [anon_sym_RPAREN] = ACTIONS(209),
    [anon_sym_COLON] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(209),
    [anon_sym_SEMI] = ACTIONS(209),
    [anon_sym_DASH_GT] = ACTIONS(209),
    [anon_sym_RBRACE] = ACTIONS(209),
    [anon_sym_COMMA] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(209),
    [sym_identifier] = ACTIONS(209),
  },
  [71] = {
    [sym__align] = ACTIONS(211),
    [anon_sym_where] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(211),
  },
  [72] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(213),
  },
  [73] = {
    [sym_path] = STATE(123),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [74] = {
    [sym_record_label] = STATE(126),
    [aux_sym_record_type_repeat1] = STATE(127),
    [anon_sym_RBRACE] = ACTIONS(219),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [75] = {
    [aux_sym_path_repeat1] = STATE(129),
    [sym__align] = ACTIONS(57),
    [ts_builtin_sym_end] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [76] = {
    [sym__align] = ACTIONS(225),
    [anon_sym_where] = ACTIONS(225),
    [ts_builtin_sym_end] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(225),
  },
  [77] = {
    [sym__align] = ACTIONS(225),
    [ts_builtin_sym_end] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(227),
  },
  [78] = {
    [sym__align] = ACTIONS(229),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(229),
  },
  [79] = {
    [aux_sym_path_repeat1] = STATE(79),
    [anon_sym_DOT] = ACTIONS(231),
    [anon_sym_EQ] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(209),
  },
  [80] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_record_expression] = STATE(114),
    [sym_record_expression_entry] = STATE(135),
    [sym_expression] = STATE(136),
    [sym_path] = STATE(137),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [aux_sym_record_expression_repeat1] = STATE(138),
    [sym_identifier] = ACTIONS(234),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(236),
    [sym_integer] = ACTIONS(191),
    [anon_sym_DOT] = ACTIONS(49),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_with] = ACTIONS(238),
    [anon_sym_let] = ACTIONS(240),
  },
  [81] = {
    [sym__align] = ACTIONS(39),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(39),
    [ts_builtin_sym_end] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(242),
    [sym_integer] = ACTIONS(39),
    [anon_sym_LPAREN] = ACTIONS(242),
    [anon_sym_let] = ACTIONS(242),
    [sym_identifier] = ACTIONS(242),
  },
  [82] = {
    [sym__align] = ACTIONS(41),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(41),
    [ts_builtin_sym_end] = ACTIONS(41),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(244),
    [sym_integer] = ACTIONS(41),
    [anon_sym_LPAREN] = ACTIONS(244),
    [anon_sym_let] = ACTIONS(244),
    [sym_identifier] = ACTIONS(244),
  },
  [83] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(140),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(246),
  },
  [84] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(141),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [85] = {
    [sym__align] = ACTIONS(248),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(248),
    [ts_builtin_sym_end] = ACTIONS(248),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(250),
    [sym_integer] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(250),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(250),
  },
  [86] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(143),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(252),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [ts_builtin_sym_end] = ACTIONS(252),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(135),
    [sym_identifier] = ACTIONS(125),
  },
  [87] = {
    [aux_sym_parameter_list_repeat1] = STATE(87),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(254),
    [sym_identifier] = ACTIONS(256),
  },
  [88] = {
    [sym_data_constructor_definition] = STATE(148),
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(147),
    [anon_sym_RBRACE] = ACTIONS(259),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(261),
  },
  [89] = {
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(150),
    [sym__end] = STATE(145),
    [sym_data_constructor_definition] = STATE(151),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(259),
    [sym_identifier] = ACTIONS(263),
  },
  [90] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(265),
  },
  [91] = {
    [sym_path] = STATE(153),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [92] = {
    [aux_sym_path_repeat1] = STATE(154),
    [anon_sym_where] = ACTIONS(57),
    [sym__align] = ACTIONS(57),
    [ts_builtin_sym_end] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [93] = {
    [anon_sym_where] = ACTIONS(225),
    [sym__align] = ACTIONS(225),
    [ts_builtin_sym_end] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(225),
    [anon_sym_DASH_GT] = ACTIONS(267),
  },
  [94] = {
    [sym_type_definition_rhs] = STATE(156),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [sym__align] = ACTIONS(269),
    [anon_sym_where] = ACTIONS(165),
    [ts_builtin_sym_end] = ACTIONS(269),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(77),
  },
  [95] = {
    [sym__align] = ACTIONS(271),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(271),
  },
  [96] = {
    [aux_sym_parameter_list_repeat1] = STATE(96),
    [anon_sym_where] = ACTIONS(273),
    [anon_sym_COLON] = ACTIONS(254),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(254),
    [sym_identifier] = ACTIONS(275),
  },
  [97] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(163),
    [sym_type] = STATE(164),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(280),
    [anon_sym_LPAREN] = ACTIONS(282),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(286),
  },
  [98] = {
    [sym__align] = ACTIONS(288),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(288),
  },
  [99] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(22),
    [sym_binding] = STATE(165),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(22),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [100] = {
    [aux_sym_path_repeat1] = STATE(40),
    [anon_sym_COLON] = ACTIONS(290),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [101] = {
    [sym_parameter_list] = STATE(168),
    [aux_sym_parameter_list_repeat1] = STATE(43),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(292),
    [sym_identifier] = ACTIONS(69),
  },
  [102] = {
    [sym_type_definition_rhs] = STATE(48),
    [aux_sym_parameter_list_repeat1] = STATE(49),
    [sym_parameter_list] = STATE(171),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(73),
    [anon_sym_COLON] = ACTIONS(294),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(296),
    [sym_identifier] = ACTIONS(79),
  },
  [103] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(143),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(298),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [ts_builtin_sym_end] = ACTIONS(298),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(135),
    [sym_identifier] = ACTIONS(125),
  },
  [104] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(172),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(135),
  },
  [105] = {
    [aux_sym_path_repeat1] = STATE(116),
    [anon_sym_DOT] = ACTIONS(105),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(179),
  },
  [106] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(300),
    [sym_identifier] = ACTIONS(300),
  },
  [107] = {
    [anon_sym_RBRACE] = ACTIONS(302),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [108] = {
    [anon_sym_where] = ACTIONS(209),
    [anon_sym_DASH_GT] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(209),
    [anon_sym_EQ] = ACTIONS(209),
  },
  [109] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_record_expression] = STATE(114),
    [sym_record_expression_entry] = STATE(176),
    [sym_expression] = STATE(177),
    [sym_path] = STATE(178),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [aux_sym_record_expression_repeat1] = STATE(179),
    [sym_identifier] = ACTIONS(234),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(304),
    [sym_integer] = ACTIONS(191),
    [anon_sym_DOT] = ACTIONS(49),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_with] = ACTIONS(306),
    [anon_sym_let] = ACTIONS(240),
  },
  [110] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(39),
    [anon_sym_RPAREN] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(242),
    [anon_sym_RBRACE] = ACTIONS(39),
    [sym_integer] = ACTIONS(39),
    [anon_sym_COMMA] = ACTIONS(39),
    [anon_sym_LPAREN] = ACTIONS(242),
    [anon_sym_let] = ACTIONS(242),
    [sym_identifier] = ACTIONS(242),
  },
  [111] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(41),
    [anon_sym_RPAREN] = ACTIONS(41),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(244),
    [anon_sym_RBRACE] = ACTIONS(41),
    [sym_integer] = ACTIONS(41),
    [anon_sym_COMMA] = ACTIONS(41),
    [anon_sym_LPAREN] = ACTIONS(244),
    [anon_sym_let] = ACTIONS(244),
    [sym_identifier] = ACTIONS(244),
  },
  [112] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(180),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(246),
  },
  [113] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(181),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [114] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(248),
    [anon_sym_RPAREN] = ACTIONS(248),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(250),
    [anon_sym_RBRACE] = ACTIONS(248),
    [sym_integer] = ACTIONS(248),
    [anon_sym_COMMA] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(250),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(250),
  },
  [115] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(182),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(183),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(308),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(308),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(199),
    [sym_identifier] = ACTIONS(189),
  },
  [116] = {
    [aux_sym_path_repeat1] = STATE(116),
    [anon_sym_DOT] = ACTIONS(310),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(209),
  },
  [117] = {
    [sym_record_expression_entry] = STATE(184),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(302),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [118] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(313),
  },
  [119] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [120] = {
    [aux_sym_path_repeat1] = STATE(186),
    [sym__align] = ACTIONS(93),
    [ts_builtin_sym_end] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [121] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(315),
  },
  [122] = {
    [aux_sym_path_repeat1] = STATE(188),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(57),
  },
  [123] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(319),
  },
  [124] = {
    [sym__align] = ACTIONS(321),
    [anon_sym_where] = ACTIONS(321),
    [ts_builtin_sym_end] = ACTIONS(321),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(321),
  },
  [125] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(323),
  },
  [126] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(325),
  },
  [127] = {
    [sym_record_label] = STATE(192),
    [aux_sym_record_type_repeat1] = STATE(193),
    [anon_sym_RBRACE] = ACTIONS(327),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [128] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(329),
  },
  [129] = {
    [aux_sym_path_repeat1] = STATE(195),
    [sym__align] = ACTIONS(93),
    [ts_builtin_sym_end] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [130] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(77),
    [sym_type] = STATE(196),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(117),
    [anon_sym_LPAREN] = ACTIONS(119),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(123),
  },
  [131] = {
    [sym_record_expression_entry] = STATE(198),
    [aux_sym_record_expression_repeat1] = STATE(199),
    [anon_sym_RBRACE] = ACTIONS(331),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [132] = {
    [sym__align] = ACTIONS(95),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(333),
    [sym_integer] = ACTIONS(95),
    [anon_sym_LPAREN] = ACTIONS(333),
    [anon_sym_let] = ACTIONS(333),
    [sym_identifier] = ACTIONS(333),
  },
  [133] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(200),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [134] = {
    [aux_sym_path_repeat1] = STATE(66),
    [sym_identifier] = ACTIONS(242),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(107),
    [anon_sym_LBRACE] = ACTIONS(242),
    [anon_sym_RBRACE] = ACTIONS(39),
    [sym_integer] = ACTIONS(39),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_LPAREN] = ACTIONS(242),
    [anon_sym_let] = ACTIONS(242),
    [anon_sym_with] = ACTIONS(335),
  },
  [135] = {
    [anon_sym_RBRACE] = ACTIONS(331),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [136] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(202),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(337),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(240),
    [sym_identifier] = ACTIONS(189),
  },
  [137] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(339),
  },
  [138] = {
    [sym_record_expression_entry] = STATE(198),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(331),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [139] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(205),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [140] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(207),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_RPAREN] = ACTIONS(341),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(246),
    [sym_identifier] = ACTIONS(189),
  },
  [141] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(343),
  },
  [142] = {
    [sym_path] = STATE(44),
    [aux_sym_application_pattern_repeat1] = STATE(44),
    [anon_sym_DOT] = ACTIONS(21),
    [anon_sym_EQ] = ACTIONS(71),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(37),
  },
  [143] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(210),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(345),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [ts_builtin_sym_end] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [sym_integer] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [144] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(143),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(349),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [ts_builtin_sym_end] = ACTIONS(349),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(135),
    [sym_identifier] = ACTIONS(125),
  },
  [145] = {
    [sym__align] = ACTIONS(351),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(351),
  },
  [146] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(353),
  },
  [147] = {
    [sym_data_constructor_definition] = STATE(214),
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(213),
    [anon_sym_RBRACE] = ACTIONS(355),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(261),
  },
  [148] = {
    [anon_sym_RBRACE] = ACTIONS(355),
    [anon_sym_SEMI] = ACTIONS(357),
    [sym_comment] = ACTIONS(3),
  },
  [149] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(359),
  },
  [150] = {
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(217),
    [sym__end] = STATE(212),
    [sym_data_constructor_definition] = STATE(218),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(355),
    [sym_identifier] = ACTIONS(263),
  },
  [151] = {
    [sym__end] = STATE(212),
    [sym__semi] = STATE(220),
    [sym__align] = ACTIONS(361),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(355),
  },
  [152] = {
    [aux_sym_path_repeat1] = STATE(221),
    [anon_sym_where] = ACTIONS(93),
    [sym__align] = ACTIONS(93),
    [ts_builtin_sym_end] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [153] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(363),
  },
  [154] = {
    [aux_sym_path_repeat1] = STATE(223),
    [anon_sym_where] = ACTIONS(93),
    [sym__align] = ACTIONS(93),
    [ts_builtin_sym_end] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [155] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(93),
    [sym_type] = STATE(196),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(153),
    [anon_sym_LPAREN] = ACTIONS(155),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(157),
  },
  [156] = {
    [sym__align] = ACTIONS(365),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(365),
  },
  [157] = {
    [anon_sym_where] = ACTIONS(211),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_COMMA] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(211),
    [anon_sym_SEMI] = ACTIONS(211),
  },
  [158] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(367),
  },
  [159] = {
    [sym_path] = STATE(225),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [160] = {
    [sym_record_label] = STATE(227),
    [aux_sym_record_type_repeat1] = STATE(228),
    [anon_sym_RBRACE] = ACTIONS(369),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [161] = {
    [aux_sym_path_repeat1] = STATE(229),
    [anon_sym_where] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [162] = {
    [anon_sym_where] = ACTIONS(225),
    [anon_sym_RBRACE] = ACTIONS(225),
    [anon_sym_COMMA] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(225),
    [anon_sym_SEMI] = ACTIONS(225),
  },
  [163] = {
    [anon_sym_where] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(225),
    [anon_sym_DASH_GT] = ACTIONS(371),
  },
  [164] = {
    [sym_type_definition_rhs] = STATE(231),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(165),
    [anon_sym_EQ] = ACTIONS(77),
    [sym_comment] = ACTIONS(3),
  },
  [165] = {
    [sym_parameter_list] = STATE(233),
    [aux_sym_parameter_list_repeat1] = STATE(43),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(373),
    [sym_identifier] = ACTIONS(69),
  },
  [166] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(237),
    [sym_type] = STATE(78),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(375),
    [anon_sym_LPAREN] = ACTIONS(377),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(379),
  },
  [167] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(239),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(381),
  },
  [168] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(373),
  },
  [169] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(243),
    [sym_type] = STATE(244),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(383),
    [anon_sym_LPAREN] = ACTIONS(385),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(387),
  },
  [170] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(237),
    [sym_type] = STATE(95),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(375),
    [anon_sym_LPAREN] = ACTIONS(377),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(379),
  },
  [171] = {
    [sym_type_definition_rhs] = STATE(98),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(165),
    [anon_sym_COLON] = ACTIONS(389),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(296),
  },
  [172] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(143),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(391),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [ts_builtin_sym_end] = ACTIONS(391),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(135),
    [sym_identifier] = ACTIONS(125),
  },
  [173] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(393),
    [sym_identifier] = ACTIONS(393),
  },
  [174] = {
    [sym_record_expression_entry] = STATE(247),
    [aux_sym_record_expression_repeat1] = STATE(248),
    [anon_sym_RBRACE] = ACTIONS(395),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [175] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(95),
    [anon_sym_RPAREN] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(333),
    [anon_sym_RBRACE] = ACTIONS(95),
    [sym_integer] = ACTIONS(95),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_LPAREN] = ACTIONS(333),
    [anon_sym_let] = ACTIONS(333),
    [sym_identifier] = ACTIONS(333),
  },
  [176] = {
    [anon_sym_RBRACE] = ACTIONS(395),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [177] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(202),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(397),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(240),
    [sym_identifier] = ACTIONS(189),
  },
  [178] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(399),
  },
  [179] = {
    [sym_record_expression_entry] = STATE(247),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(395),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [180] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(207),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_RPAREN] = ACTIONS(401),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(246),
    [sym_identifier] = ACTIONS(189),
  },
  [181] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(403),
  },
  [182] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(253),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(183),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [anon_sym_RBRACE] = ACTIONS(345),
    [sym_integer] = ACTIONS(345),
    [anon_sym_COMMA] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [183] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(182),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(183),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(349),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(349),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(199),
    [sym_identifier] = ACTIONS(189),
  },
  [184] = {
    [anon_sym_RBRACE] = ACTIONS(405),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [185] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(256),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(407),
  },
  [186] = {
    [aux_sym_path_repeat1] = STATE(195),
    [sym__align] = ACTIONS(179),
    [ts_builtin_sym_end] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [187] = {
    [aux_sym_path_repeat1] = STATE(257),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(93),
  },
  [188] = {
    [aux_sym_path_repeat1] = STATE(258),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(93),
  },
  [189] = {
    [sym_path] = STATE(261),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [190] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(266),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [191] = {
    [sym__align] = ACTIONS(419),
    [anon_sym_where] = ACTIONS(419),
    [ts_builtin_sym_end] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(419),
  },
  [192] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(421),
  },
  [193] = {
    [sym_record_label] = STATE(268),
    [aux_sym_record_type_repeat1] = STATE(193),
    [anon_sym_RBRACE] = ACTIONS(423),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(425),
  },
  [194] = {
    [sym__align] = ACTIONS(209),
    [anon_sym_where] = ACTIONS(209),
    [ts_builtin_sym_end] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(209),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [195] = {
    [aux_sym_path_repeat1] = STATE(195),
    [sym__align] = ACTIONS(209),
    [ts_builtin_sym_end] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(428),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [196] = {
    [sym__align] = ACTIONS(431),
    [anon_sym_where] = ACTIONS(431),
    [ts_builtin_sym_end] = ACTIONS(431),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(431),
  },
  [197] = {
    [sym__align] = ACTIONS(183),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(183),
    [ts_builtin_sym_end] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(433),
    [sym_integer] = ACTIONS(183),
    [anon_sym_LPAREN] = ACTIONS(433),
    [anon_sym_let] = ACTIONS(433),
    [sym_identifier] = ACTIONS(433),
  },
  [198] = {
    [anon_sym_RBRACE] = ACTIONS(435),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [199] = {
    [sym_record_expression_entry] = STATE(270),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(435),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [200] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(437),
  },
  [201] = {
    [sym__align] = ACTIONS(439),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(439),
    [ts_builtin_sym_end] = ACTIONS(439),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(441),
    [sym_integer] = ACTIONS(439),
    [anon_sym_LPAREN] = ACTIONS(441),
    [anon_sym_let] = ACTIONS(441),
    [sym_identifier] = ACTIONS(441),
  },
  [202] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(272),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [anon_sym_RBRACE] = ACTIONS(345),
    [sym_integer] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [203] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(202),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(349),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(240),
    [sym_identifier] = ACTIONS(189),
  },
  [204] = {
    [sym_record_expression_entry] = STATE(270),
    [aux_sym_record_expression_repeat1] = STATE(273),
    [anon_sym_RBRACE] = ACTIONS(435),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [205] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(443),
  },
  [206] = {
    [sym__align] = ACTIONS(445),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(445),
    [ts_builtin_sym_end] = ACTIONS(445),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(447),
    [sym_integer] = ACTIONS(445),
    [anon_sym_LPAREN] = ACTIONS(447),
    [anon_sym_let] = ACTIONS(447),
    [sym_identifier] = ACTIONS(447),
  },
  [207] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(275),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [anon_sym_RPAREN] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [sym_integer] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [208] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(207),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_RPAREN] = ACTIONS(349),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(246),
    [sym_identifier] = ACTIONS(189),
  },
  [209] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(282),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [210] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(210),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(461),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(463),
    [ts_builtin_sym_end] = ACTIONS(461),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(466),
    [sym_integer] = ACTIONS(469),
    [anon_sym_LPAREN] = ACTIONS(472),
    [anon_sym_let] = ACTIONS(475),
    [sym_identifier] = ACTIONS(478),
  },
  [211] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(286),
    [sym_type] = STATE(287),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(481),
    [anon_sym_LPAREN] = ACTIONS(483),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(485),
  },
  [212] = {
    [sym__align] = ACTIONS(487),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(487),
  },
  [213] = {
    [sym_data_constructor_definition] = STATE(289),
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(213),
    [anon_sym_RBRACE] = ACTIONS(489),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(491),
  },
  [214] = {
    [anon_sym_RBRACE] = ACTIONS(494),
    [anon_sym_SEMI] = ACTIONS(357),
    [sym_comment] = ACTIONS(3),
  },
  [215] = {
    [anon_sym_RBRACE] = ACTIONS(489),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(489),
  },
  [216] = {
    [sym_record_type] = STATE(296),
    [sym_path] = STATE(297),
    [sym_type] = STATE(298),
    [sym_function_type] = STATE(296),
    [sym_unit_type] = STATE(296),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(496),
    [anon_sym_DOT] = ACTIONS(498),
    [anon_sym_LPAREN] = ACTIONS(500),
    [anon_sym_LBRACE] = ACTIONS(502),
    [sym_identifier] = ACTIONS(504),
  },
  [217] = {
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(217),
    [sym_data_constructor_definition] = STATE(300),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(506),
    [sym_identifier] = ACTIONS(508),
  },
  [218] = {
    [sym__end] = STATE(290),
    [sym__semi] = STATE(220),
    [sym__align] = ACTIONS(361),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(494),
  },
  [219] = {
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(511),
    [sym_identifier] = ACTIONS(511),
  },
  [220] = {
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(506),
    [sym_identifier] = ACTIONS(506),
  },
  [221] = {
    [aux_sym_path_repeat1] = STATE(223),
    [anon_sym_where] = ACTIONS(179),
    [sym__align] = ACTIONS(179),
    [ts_builtin_sym_end] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [222] = {
    [sym_path] = STATE(301),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [223] = {
    [aux_sym_path_repeat1] = STATE(223),
    [anon_sym_where] = ACTIONS(209),
    [sym__align] = ACTIONS(209),
    [ts_builtin_sym_end] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(428),
    [anon_sym_EQ] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [224] = {
    [aux_sym_path_repeat1] = STATE(302),
    [anon_sym_where] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [225] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(513),
  },
  [226] = {
    [anon_sym_where] = ACTIONS(321),
    [anon_sym_RBRACE] = ACTIONS(321),
    [anon_sym_COMMA] = ACTIONS(321),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(321),
    [anon_sym_SEMI] = ACTIONS(321),
  },
  [227] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(515),
  },
  [228] = {
    [sym_record_label] = STATE(306),
    [aux_sym_record_type_repeat1] = STATE(193),
    [anon_sym_RBRACE] = ACTIONS(517),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [229] = {
    [aux_sym_path_repeat1] = STATE(307),
    [anon_sym_where] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [230] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(163),
    [sym_type] = STATE(308),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(280),
    [anon_sym_LPAREN] = ACTIONS(282),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(286),
  },
  [231] = {
    [sym__align] = ACTIONS(519),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(519),
  },
  [232] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(309),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(381),
  },
  [233] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(521),
  },
  [234] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(523),
  },
  [235] = {
    [sym_path] = STATE(312),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [236] = {
    [aux_sym_path_repeat1] = STATE(313),
    [sym__align] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [237] = {
    [sym__align] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(525),
  },
  [238] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(315),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [239] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(316),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(252),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(381),
    [sym_identifier] = ACTIONS(125),
  },
  [240] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(527),
  },
  [241] = {
    [sym_path] = STATE(319),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [242] = {
    [aux_sym_path_repeat1] = STATE(320),
    [anon_sym_where] = ACTIONS(57),
    [sym__align] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [243] = {
    [anon_sym_where] = ACTIONS(225),
    [sym__align] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(225),
    [anon_sym_DASH_GT] = ACTIONS(529),
  },
  [244] = {
    [sym_type_definition_rhs] = STATE(156),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [sym__align] = ACTIONS(269),
    [anon_sym_where] = ACTIONS(165),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(296),
  },
  [245] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(163),
    [sym_type] = STATE(322),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(280),
    [anon_sym_LPAREN] = ACTIONS(282),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(286),
  },
  [246] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(183),
    [anon_sym_RPAREN] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(433),
    [anon_sym_RBRACE] = ACTIONS(183),
    [sym_integer] = ACTIONS(183),
    [anon_sym_COMMA] = ACTIONS(183),
    [anon_sym_LPAREN] = ACTIONS(433),
    [anon_sym_let] = ACTIONS(433),
    [sym_identifier] = ACTIONS(433),
  },
  [247] = {
    [anon_sym_RBRACE] = ACTIONS(531),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [248] = {
    [sym_record_expression_entry] = STATE(324),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(531),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [249] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(439),
    [anon_sym_RPAREN] = ACTIONS(439),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(441),
    [anon_sym_RBRACE] = ACTIONS(439),
    [sym_integer] = ACTIONS(439),
    [anon_sym_COMMA] = ACTIONS(439),
    [anon_sym_LPAREN] = ACTIONS(441),
    [anon_sym_let] = ACTIONS(441),
    [sym_identifier] = ACTIONS(441),
  },
  [250] = {
    [sym_record_expression_entry] = STATE(324),
    [aux_sym_record_expression_repeat1] = STATE(325),
    [anon_sym_RBRACE] = ACTIONS(531),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [251] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(445),
    [anon_sym_RPAREN] = ACTIONS(445),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(447),
    [anon_sym_RBRACE] = ACTIONS(445),
    [sym_integer] = ACTIONS(445),
    [anon_sym_COMMA] = ACTIONS(445),
    [anon_sym_LPAREN] = ACTIONS(447),
    [anon_sym_let] = ACTIONS(447),
    [sym_identifier] = ACTIONS(447),
  },
  [252] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(326),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [253] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(253),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(183),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(533),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(536),
    [anon_sym_RBRACE] = ACTIONS(461),
    [sym_integer] = ACTIONS(539),
    [anon_sym_COMMA] = ACTIONS(461),
    [anon_sym_LPAREN] = ACTIONS(542),
    [anon_sym_let] = ACTIONS(545),
    [sym_identifier] = ACTIONS(548),
  },
  [254] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(551),
    [sym_identifier] = ACTIONS(551),
  },
  [255] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(327),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [256] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(328),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(329),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(308),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(407),
    [sym_identifier] = ACTIONS(189),
  },
  [257] = {
    [aux_sym_path_repeat1] = STATE(258),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(179),
  },
  [258] = {
    [aux_sym_path_repeat1] = STATE(258),
    [anon_sym_DOT] = ACTIONS(231),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(209),
  },
  [259] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(553),
  },
  [260] = {
    [aux_sym_path_repeat1] = STATE(331),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(57),
  },
  [261] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(555),
  },
  [262] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(557),
  },
  [263] = {
    [sym_path] = STATE(334),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [264] = {
    [aux_sym_path_repeat1] = STATE(335),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [265] = {
    [anon_sym_RBRACE] = ACTIONS(225),
    [anon_sym_COMMA] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(559),
  },
  [266] = {
    [anon_sym_RBRACE] = ACTIONS(561),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [267] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(339),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [268] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(565),
  },
  [269] = {
    [sym__align] = ACTIONS(300),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(300),
    [ts_builtin_sym_end] = ACTIONS(300),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(567),
    [sym_integer] = ACTIONS(300),
    [anon_sym_LPAREN] = ACTIONS(567),
    [anon_sym_let] = ACTIONS(567),
    [sym_identifier] = ACTIONS(567),
  },
  [270] = {
    [anon_sym_RBRACE] = ACTIONS(569),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [271] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(342),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [272] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(272),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(533),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(536),
    [anon_sym_RBRACE] = ACTIONS(461),
    [sym_integer] = ACTIONS(539),
    [anon_sym_LPAREN] = ACTIONS(542),
    [anon_sym_let] = ACTIONS(571),
    [sym_identifier] = ACTIONS(548),
  },
  [273] = {
    [sym_record_expression_entry] = STATE(343),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(569),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [274] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(344),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [275] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(275),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(533),
    [anon_sym_RPAREN] = ACTIONS(461),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(536),
    [sym_integer] = ACTIONS(539),
    [anon_sym_LPAREN] = ACTIONS(542),
    [anon_sym_let] = ACTIONS(574),
    [sym_identifier] = ACTIONS(548),
  },
  [276] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_record_expression] = STATE(114),
    [sym_record_expression_entry] = STATE(347),
    [sym_expression] = STATE(348),
    [sym_path] = STATE(349),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [aux_sym_record_expression_repeat1] = STATE(350),
    [sym_identifier] = ACTIONS(234),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(577),
    [sym_integer] = ACTIONS(191),
    [anon_sym_DOT] = ACTIONS(49),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_with] = ACTIONS(579),
    [anon_sym_let] = ACTIONS(240),
  },
  [277] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(39),
    [anon_sym_in] = ACTIONS(242),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(242),
    [sym_integer] = ACTIONS(39),
    [anon_sym_LPAREN] = ACTIONS(242),
    [anon_sym_let] = ACTIONS(242),
    [sym_identifier] = ACTIONS(242),
  },
  [278] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(41),
    [anon_sym_in] = ACTIONS(244),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(244),
    [sym_integer] = ACTIONS(41),
    [anon_sym_LPAREN] = ACTIONS(244),
    [anon_sym_let] = ACTIONS(244),
    [sym_identifier] = ACTIONS(244),
  },
  [279] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(351),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(246),
  },
  [280] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [sym_record_expression] = STATE(20),
    [sym_path] = STATE(142),
    [sym_binding] = STATE(352),
    [sym_unit] = STATE(12),
    [aux_sym_application_pattern_repeat1] = STATE(142),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [281] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(248),
    [anon_sym_in] = ACTIONS(250),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(250),
    [sym_integer] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(250),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(250),
  },
  [282] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(581),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [283] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(583),
  },
  [284] = {
    [sym_path] = STATE(357),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [285] = {
    [aux_sym_path_repeat1] = STATE(358),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [286] = {
    [anon_sym_RBRACE] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(225),
    [anon_sym_DASH_GT] = ACTIONS(585),
  },
  [287] = {
    [anon_sym_RBRACE] = ACTIONS(587),
    [anon_sym_SEMI] = ACTIONS(587),
    [sym_comment] = ACTIONS(3),
  },
  [288] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(589),
  },
  [289] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(357),
  },
  [290] = {
    [sym__align] = ACTIONS(591),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(591),
  },
  [291] = {
    [sym__align] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(211),
  },
  [292] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(593),
  },
  [293] = {
    [sym_path] = STATE(362),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [294] = {
    [sym_record_label] = STATE(364),
    [aux_sym_record_type_repeat1] = STATE(365),
    [anon_sym_RBRACE] = ACTIONS(595),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [295] = {
    [aux_sym_path_repeat1] = STATE(367),
    [sym__align] = ACTIONS(57),
    [sym__dedent] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(597),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [296] = {
    [sym__align] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(225),
  },
  [297] = {
    [sym__align] = ACTIONS(225),
    [sym__dedent] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(599),
  },
  [298] = {
    [sym__align] = ACTIONS(587),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(587),
  },
  [299] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(601),
  },
  [300] = {
    [sym__semi] = STATE(220),
    [sym__align] = ACTIONS(361),
    [sym_comment] = ACTIONS(3),
  },
  [301] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(603),
  },
  [302] = {
    [aux_sym_path_repeat1] = STATE(307),
    [anon_sym_where] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(105),
    [anon_sym_EQ] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [303] = {
    [sym_path] = STATE(371),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [304] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(372),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [305] = {
    [anon_sym_where] = ACTIONS(419),
    [anon_sym_RBRACE] = ACTIONS(419),
    [anon_sym_COMMA] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(419),
    [anon_sym_SEMI] = ACTIONS(419),
  },
  [306] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(605),
  },
  [307] = {
    [aux_sym_path_repeat1] = STATE(307),
    [anon_sym_where] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(310),
    [anon_sym_EQ] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [308] = {
    [anon_sym_where] = ACTIONS(431),
    [anon_sym_RBRACE] = ACTIONS(431),
    [anon_sym_COMMA] = ACTIONS(431),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(431),
    [anon_sym_SEMI] = ACTIONS(431),
  },
  [309] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(316),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(298),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(381),
    [sym_identifier] = ACTIONS(125),
  },
  [310] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(374),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(381),
  },
  [311] = {
    [aux_sym_path_repeat1] = STATE(375),
    [sym__align] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [312] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(607),
  },
  [313] = {
    [aux_sym_path_repeat1] = STATE(377),
    [sym__align] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [314] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(237),
    [sym_type] = STATE(196),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(375),
    [anon_sym_LPAREN] = ACTIONS(377),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(379),
  },
  [315] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(609),
  },
  [316] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(379),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(345),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [sym_integer] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [317] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(316),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(349),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(381),
    [sym_identifier] = ACTIONS(125),
  },
  [318] = {
    [aux_sym_path_repeat1] = STATE(380),
    [anon_sym_where] = ACTIONS(93),
    [sym__align] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [319] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(611),
  },
  [320] = {
    [aux_sym_path_repeat1] = STATE(382),
    [anon_sym_where] = ACTIONS(93),
    [sym__align] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [321] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(243),
    [sym_type] = STATE(196),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(383),
    [anon_sym_LPAREN] = ACTIONS(385),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(387),
  },
  [322] = {
    [sym_type_definition_rhs] = STATE(231),
    [sym_data_type_definition_rhs] = STATE(51),
    [sym_type_alias_definition_rhs] = STATE(51),
    [anon_sym_where] = ACTIONS(165),
    [anon_sym_EQ] = ACTIONS(296),
    [sym_comment] = ACTIONS(3),
  },
  [323] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(300),
    [anon_sym_RPAREN] = ACTIONS(300),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(567),
    [anon_sym_RBRACE] = ACTIONS(300),
    [sym_integer] = ACTIONS(300),
    [anon_sym_COMMA] = ACTIONS(300),
    [anon_sym_LPAREN] = ACTIONS(567),
    [anon_sym_let] = ACTIONS(567),
    [sym_identifier] = ACTIONS(567),
  },
  [324] = {
    [anon_sym_RBRACE] = ACTIONS(613),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [325] = {
    [sym_record_expression_entry] = STATE(384),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(613),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [326] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(615),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [327] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(617),
  },
  [328] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(387),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(329),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [sym_integer] = ACTIONS(345),
    [anon_sym_COMMA] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [329] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(328),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(329),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(349),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(407),
    [sym_identifier] = ACTIONS(189),
  },
  [330] = {
    [aux_sym_path_repeat1] = STATE(388),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(93),
  },
  [331] = {
    [aux_sym_path_repeat1] = STATE(389),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(93),
  },
  [332] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(619),
  },
  [333] = {
    [aux_sym_path_repeat1] = STATE(391),
    [anon_sym_RBRACE] = ACTIONS(93),
    [anon_sym_COMMA] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [334] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(621),
  },
  [335] = {
    [aux_sym_path_repeat1] = STATE(393),
    [anon_sym_RBRACE] = ACTIONS(93),
    [anon_sym_COMMA] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [336] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(308),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [337] = {
    [sym__align] = ACTIONS(623),
    [anon_sym_where] = ACTIONS(623),
    [ts_builtin_sym_end] = ACTIONS(623),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(623),
  },
  [338] = {
    [anon_sym_RBRACE] = ACTIONS(625),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(625),
  },
  [339] = {
    [anon_sym_RBRACE] = ACTIONS(627),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [340] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(398),
    [sym_type] = STATE(399),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(629),
    [anon_sym_LPAREN] = ACTIONS(631),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(633),
  },
  [341] = {
    [sym__align] = ACTIONS(393),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(393),
    [ts_builtin_sym_end] = ACTIONS(393),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(635),
    [sym_integer] = ACTIONS(393),
    [anon_sym_LPAREN] = ACTIONS(635),
    [anon_sym_let] = ACTIONS(635),
    [sym_identifier] = ACTIONS(635),
  },
  [342] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(637),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [343] = {
    [anon_sym_RBRACE] = ACTIONS(639),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [344] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(641),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [345] = {
    [sym_record_expression_entry] = STATE(404),
    [aux_sym_record_expression_repeat1] = STATE(405),
    [anon_sym_RBRACE] = ACTIONS(643),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [346] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(95),
    [anon_sym_in] = ACTIONS(333),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(333),
    [sym_integer] = ACTIONS(95),
    [anon_sym_LPAREN] = ACTIONS(333),
    [anon_sym_let] = ACTIONS(333),
    [sym_identifier] = ACTIONS(333),
  },
  [347] = {
    [anon_sym_RBRACE] = ACTIONS(643),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [348] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(202),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(645),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(240),
    [sym_identifier] = ACTIONS(189),
  },
  [349] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(647),
  },
  [350] = {
    [sym_record_expression_entry] = STATE(404),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(643),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [351] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(207),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_RPAREN] = ACTIONS(649),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(246),
    [sym_identifier] = ACTIONS(189),
  },
  [352] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(651),
  },
  [353] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(410),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(135),
  },
  [354] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(411),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(345),
    [anon_sym_in] = ACTIONS(347),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(347),
    [sym_integer] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(347),
    [anon_sym_let] = ACTIONS(347),
    [sym_identifier] = ACTIONS(347),
  },
  [355] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(653),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [356] = {
    [aux_sym_path_repeat1] = STATE(412),
    [anon_sym_RBRACE] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [357] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(655),
  },
  [358] = {
    [aux_sym_path_repeat1] = STATE(414),
    [anon_sym_RBRACE] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [359] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(286),
    [sym_type] = STATE(308),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(481),
    [anon_sym_LPAREN] = ACTIONS(483),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(485),
  },
  [360] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(418),
    [sym_type] = STATE(287),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(657),
    [anon_sym_LPAREN] = ACTIONS(659),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(661),
  },
  [361] = {
    [aux_sym_path_repeat1] = STATE(419),
    [sym__align] = ACTIONS(93),
    [sym__dedent] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(597),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [362] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(663),
  },
  [363] = {
    [sym__align] = ACTIONS(321),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(321),
  },
  [364] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(665),
  },
  [365] = {
    [sym_record_label] = STATE(423),
    [aux_sym_record_type_repeat1] = STATE(193),
    [anon_sym_RBRACE] = ACTIONS(667),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(221),
  },
  [366] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(669),
  },
  [367] = {
    [aux_sym_path_repeat1] = STATE(425),
    [sym__align] = ACTIONS(93),
    [sym__dedent] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(597),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [368] = {
    [sym_record_type] = STATE(296),
    [sym_path] = STATE(297),
    [sym_type] = STATE(426),
    [sym_function_type] = STATE(296),
    [sym_unit_type] = STATE(296),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(496),
    [anon_sym_DOT] = ACTIONS(498),
    [anon_sym_LPAREN] = ACTIONS(500),
    [anon_sym_LBRACE] = ACTIONS(502),
    [sym_identifier] = ACTIONS(504),
  },
  [369] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(237),
    [sym_type] = STATE(427),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(375),
    [anon_sym_LPAREN] = ACTIONS(377),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(379),
  },
  [370] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(671),
  },
  [371] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(673),
  },
  [372] = {
    [anon_sym_RBRACE] = ACTIONS(675),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [373] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(431),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [374] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(316),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(391),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(381),
    [sym_identifier] = ACTIONS(125),
  },
  [375] = {
    [aux_sym_path_repeat1] = STATE(377),
    [sym__align] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(223),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [376] = {
    [sym_path] = STATE(432),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [377] = {
    [aux_sym_path_repeat1] = STATE(377),
    [sym__align] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(428),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [378] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(433),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [379] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(379),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(461),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(463),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(466),
    [sym_integer] = ACTIONS(469),
    [anon_sym_LPAREN] = ACTIONS(472),
    [anon_sym_let] = ACTIONS(677),
    [sym_identifier] = ACTIONS(478),
  },
  [380] = {
    [aux_sym_path_repeat1] = STATE(382),
    [anon_sym_where] = ACTIONS(179),
    [sym__align] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(223),
    [anon_sym_EQ] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [381] = {
    [sym_path] = STATE(434),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [382] = {
    [aux_sym_path_repeat1] = STATE(382),
    [anon_sym_where] = ACTIONS(209),
    [sym__align] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(428),
    [anon_sym_EQ] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [383] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(393),
    [anon_sym_RPAREN] = ACTIONS(393),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(635),
    [anon_sym_RBRACE] = ACTIONS(393),
    [sym_integer] = ACTIONS(393),
    [anon_sym_COMMA] = ACTIONS(393),
    [anon_sym_LPAREN] = ACTIONS(635),
    [anon_sym_let] = ACTIONS(635),
    [sym_identifier] = ACTIONS(635),
  },
  [384] = {
    [anon_sym_RBRACE] = ACTIONS(680),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [385] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(436),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(199),
  },
  [386] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(437),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [387] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(387),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(329),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(533),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(536),
    [sym_integer] = ACTIONS(539),
    [anon_sym_COMMA] = ACTIONS(461),
    [anon_sym_LPAREN] = ACTIONS(542),
    [anon_sym_let] = ACTIONS(682),
    [sym_identifier] = ACTIONS(548),
  },
  [388] = {
    [aux_sym_path_repeat1] = STATE(389),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(179),
  },
  [389] = {
    [aux_sym_path_repeat1] = STATE(389),
    [anon_sym_DOT] = ACTIONS(231),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(209),
  },
  [390] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(77),
    [sym_type] = STATE(438),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(117),
    [anon_sym_LPAREN] = ACTIONS(119),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(123),
  },
  [391] = {
    [aux_sym_path_repeat1] = STATE(393),
    [anon_sym_RBRACE] = ACTIONS(179),
    [anon_sym_COMMA] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [392] = {
    [sym_path] = STATE(439),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [393] = {
    [aux_sym_path_repeat1] = STATE(393),
    [anon_sym_RBRACE] = ACTIONS(209),
    [anon_sym_COMMA] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(231),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [394] = {
    [sym__align] = ACTIONS(685),
    [anon_sym_where] = ACTIONS(685),
    [ts_builtin_sym_end] = ACTIONS(685),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(685),
  },
  [395] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(687),
  },
  [396] = {
    [sym_path] = STATE(441),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [397] = {
    [aux_sym_path_repeat1] = STATE(442),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [398] = {
    [anon_sym_DASH_GT] = ACTIONS(689),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(225),
  },
  [399] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [400] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(444),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(240),
  },
  [401] = {
    [sym__align] = ACTIONS(551),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(551),
    [ts_builtin_sym_end] = ACTIONS(551),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(691),
    [sym_integer] = ACTIONS(551),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_let] = ACTIONS(691),
    [sym_identifier] = ACTIONS(691),
  },
  [402] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(445),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(246),
  },
  [403] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(183),
    [anon_sym_in] = ACTIONS(433),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(433),
    [sym_integer] = ACTIONS(183),
    [anon_sym_LPAREN] = ACTIONS(433),
    [anon_sym_let] = ACTIONS(433),
    [sym_identifier] = ACTIONS(433),
  },
  [404] = {
    [anon_sym_RBRACE] = ACTIONS(693),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [405] = {
    [sym_record_expression_entry] = STATE(447),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(693),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [406] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(439),
    [anon_sym_in] = ACTIONS(441),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(441),
    [sym_integer] = ACTIONS(439),
    [anon_sym_LPAREN] = ACTIONS(441),
    [anon_sym_let] = ACTIONS(441),
    [sym_identifier] = ACTIONS(441),
  },
  [407] = {
    [sym_record_expression_entry] = STATE(447),
    [aux_sym_record_expression_repeat1] = STATE(448),
    [anon_sym_RBRACE] = ACTIONS(693),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [408] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(445),
    [anon_sym_in] = ACTIONS(447),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(447),
    [sym_integer] = ACTIONS(445),
    [anon_sym_LPAREN] = ACTIONS(447),
    [anon_sym_let] = ACTIONS(447),
    [sym_identifier] = ACTIONS(447),
  },
  [409] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(449),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [410] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(143),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(695),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [ts_builtin_sym_end] = ACTIONS(695),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(135),
    [sym_identifier] = ACTIONS(125),
  },
  [411] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(411),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(697),
    [anon_sym_in] = ACTIONS(700),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(702),
    [sym_integer] = ACTIONS(705),
    [anon_sym_LPAREN] = ACTIONS(708),
    [anon_sym_let] = ACTIONS(711),
    [sym_identifier] = ACTIONS(714),
  },
  [412] = {
    [aux_sym_path_repeat1] = STATE(414),
    [anon_sym_RBRACE] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [413] = {
    [sym_path] = STATE(450),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [414] = {
    [aux_sym_path_repeat1] = STATE(414),
    [anon_sym_RBRACE] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(231),
    [anon_sym_SEMI] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [415] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(717),
  },
  [416] = {
    [sym_path] = STATE(452),
    [anon_sym_DOT] = ACTIONS(215),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(217),
  },
  [417] = {
    [aux_sym_path_repeat1] = STATE(453),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [418] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(225),
    [anon_sym_DASH_GT] = ACTIONS(719),
  },
  [419] = {
    [aux_sym_path_repeat1] = STATE(425),
    [sym__align] = ACTIONS(179),
    [sym__dedent] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(597),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [420] = {
    [sym_path] = STATE(455),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [421] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(456),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [422] = {
    [sym__align] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(419),
  },
  [423] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(721),
  },
  [424] = {
    [sym__align] = ACTIONS(209),
    [sym__dedent] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [425] = {
    [aux_sym_path_repeat1] = STATE(425),
    [sym__align] = ACTIONS(209),
    [sym__dedent] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(723),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [426] = {
    [sym__align] = ACTIONS(431),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(431),
  },
  [427] = {
    [sym__align] = ACTIONS(587),
    [sym_comment] = ACTIONS(3),
  },
  [428] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(93),
    [sym_type] = STATE(438),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(153),
    [anon_sym_LPAREN] = ACTIONS(155),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(157),
  },
  [429] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(726),
  },
  [430] = {
    [anon_sym_where] = ACTIONS(623),
    [anon_sym_RBRACE] = ACTIONS(623),
    [anon_sym_COMMA] = ACTIONS(623),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(623),
    [anon_sym_SEMI] = ACTIONS(623),
  },
  [431] = {
    [anon_sym_RBRACE] = ACTIONS(728),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [432] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(730),
  },
  [433] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(732),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [434] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(734),
  },
  [435] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(551),
    [anon_sym_RPAREN] = ACTIONS(551),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(691),
    [anon_sym_RBRACE] = ACTIONS(551),
    [sym_integer] = ACTIONS(551),
    [anon_sym_COMMA] = ACTIONS(551),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_let] = ACTIONS(691),
    [sym_identifier] = ACTIONS(691),
  },
  [436] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(182),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(183),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(695),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(695),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(199),
    [sym_identifier] = ACTIONS(189),
  },
  [437] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(736),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [438] = {
    [sym__align] = ACTIONS(738),
    [anon_sym_where] = ACTIONS(738),
    [ts_builtin_sym_end] = ACTIONS(738),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(738),
  },
  [439] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(740),
  },
  [440] = {
    [aux_sym_path_repeat1] = STATE(465),
    [anon_sym_COMMA] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [441] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(742),
  },
  [442] = {
    [aux_sym_path_repeat1] = STATE(467),
    [anon_sym_COMMA] = ACTIONS(93),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [443] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(398),
    [sym_type] = STATE(308),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(629),
    [anon_sym_LPAREN] = ACTIONS(631),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(633),
  },
  [444] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(202),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(203),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_RBRACE] = ACTIONS(695),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(240),
    [sym_identifier] = ACTIONS(189),
  },
  [445] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(207),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(208),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [anon_sym_RPAREN] = ACTIONS(695),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(246),
    [sym_identifier] = ACTIONS(189),
  },
  [446] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(300),
    [anon_sym_in] = ACTIONS(567),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(567),
    [sym_integer] = ACTIONS(300),
    [anon_sym_LPAREN] = ACTIONS(567),
    [anon_sym_let] = ACTIONS(567),
    [sym_identifier] = ACTIONS(567),
  },
  [447] = {
    [anon_sym_RBRACE] = ACTIONS(744),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [448] = {
    [sym_record_expression_entry] = STATE(469),
    [aux_sym_record_expression_repeat1] = STATE(69),
    [anon_sym_RBRACE] = ACTIONS(744),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(103),
  },
  [449] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(746),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [450] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(748),
  },
  [451] = {
    [aux_sym_path_repeat1] = STATE(472),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [452] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(750),
  },
  [453] = {
    [aux_sym_path_repeat1] = STATE(474),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(93),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(93),
  },
  [454] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(418),
    [sym_type] = STATE(308),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(657),
    [anon_sym_LPAREN] = ACTIONS(659),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(661),
  },
  [455] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(752),
  },
  [456] = {
    [anon_sym_RBRACE] = ACTIONS(754),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [457] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(477),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [458] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(163),
    [sym_type] = STATE(478),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(280),
    [anon_sym_LPAREN] = ACTIONS(282),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(286),
  },
  [459] = {
    [anon_sym_where] = ACTIONS(685),
    [anon_sym_RBRACE] = ACTIONS(685),
    [anon_sym_COMMA] = ACTIONS(685),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(685),
    [anon_sym_SEMI] = ACTIONS(685),
  },
  [460] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(756),
  },
  [461] = {
    [sym_record_expression] = STATE(85),
    [sym_let_binding] = STATE(85),
    [sym_expression] = STATE(480),
    [sym_suspension] = STATE(85),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [sym_identifier] = ACTIONS(125),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_LBRACE] = ACTIONS(133),
    [anon_sym_let] = ACTIONS(381),
  },
  [462] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(758),
  },
  [463] = {
    [sym_record_expression] = STATE(114),
    [sym_let_binding] = STATE(114),
    [sym_expression] = STATE(482),
    [sym_suspension] = STATE(114),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [sym_identifier] = ACTIONS(189),
    [sym_integer] = ACTIONS(191),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_LBRACE] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(407),
  },
  [464] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(760),
  },
  [465] = {
    [aux_sym_path_repeat1] = STATE(467),
    [anon_sym_COMMA] = ACTIONS(179),
    [anon_sym_DOT] = ACTIONS(317),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [466] = {
    [sym_path] = STATE(484),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [467] = {
    [aux_sym_path_repeat1] = STATE(467),
    [anon_sym_COMMA] = ACTIONS(209),
    [anon_sym_DOT] = ACTIONS(231),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [468] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(393),
    [anon_sym_in] = ACTIONS(635),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(635),
    [sym_integer] = ACTIONS(393),
    [anon_sym_LPAREN] = ACTIONS(635),
    [anon_sym_let] = ACTIONS(635),
    [sym_identifier] = ACTIONS(635),
  },
  [469] = {
    [anon_sym_RBRACE] = ACTIONS(762),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [470] = {
    [sym_record_expression] = STATE(281),
    [sym_let_binding] = STATE(281),
    [sym_expression] = STATE(486),
    [sym_suspension] = STATE(281),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [sym_identifier] = ACTIONS(449),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_LBRACE] = ACTIONS(457),
    [anon_sym_let] = ACTIONS(459),
  },
  [471] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(764),
  },
  [472] = {
    [aux_sym_path_repeat1] = STATE(474),
    [anon_sym_DOT] = ACTIONS(317),
    [anon_sym_SEMI] = ACTIONS(179),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(179),
  },
  [473] = {
    [sym_path] = STATE(488),
    [anon_sym_DOT] = ACTIONS(409),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(411),
  },
  [474] = {
    [aux_sym_path_repeat1] = STATE(474),
    [anon_sym_DOT] = ACTIONS(231),
    [anon_sym_SEMI] = ACTIONS(209),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(209),
  },
  [475] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(766),
  },
  [476] = {
    [sym__align] = ACTIONS(623),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(623),
  },
  [477] = {
    [anon_sym_RBRACE] = ACTIONS(768),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(563),
  },
  [478] = {
    [anon_sym_where] = ACTIONS(738),
    [anon_sym_RBRACE] = ACTIONS(738),
    [anon_sym_COMMA] = ACTIONS(738),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(738),
    [anon_sym_SEMI] = ACTIONS(738),
  },
  [479] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(237),
    [sym_type] = STATE(438),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(375),
    [anon_sym_LPAREN] = ACTIONS(377),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(379),
  },
  [480] = {
    [sym_let_binding] = STATE(85),
    [sym_suspension] = STATE(85),
    [sym_atomic_expression] = STATE(85),
    [aux_sym_application_expression_repeat1] = STATE(316),
    [sym_record_expression] = STATE(85),
    [sym_expression] = STATE(317),
    [sym_unit] = STATE(81),
    [sym_application_expression] = STATE(85),
    [sym__align] = ACTIONS(695),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(129),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(133),
    [sym_integer] = ACTIONS(127),
    [anon_sym_LPAREN] = ACTIONS(131),
    [anon_sym_let] = ACTIONS(381),
    [sym_identifier] = ACTIONS(125),
  },
  [481] = {
    [sym_record_type] = STATE(76),
    [sym_path] = STATE(243),
    [sym_type] = STATE(438),
    [sym_function_type] = STATE(76),
    [sym_unit_type] = STATE(76),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(115),
    [anon_sym_DOT] = ACTIONS(383),
    [anon_sym_LPAREN] = ACTIONS(385),
    [anon_sym_LBRACE] = ACTIONS(121),
    [sym_identifier] = ACTIONS(387),
  },
  [482] = {
    [sym_let_binding] = STATE(114),
    [sym_suspension] = STATE(114),
    [sym_atomic_expression] = STATE(114),
    [aux_sym_application_expression_repeat1] = STATE(328),
    [sym_record_expression] = STATE(114),
    [sym_expression] = STATE(329),
    [sym_unit] = STATE(110),
    [sym_application_expression] = STATE(114),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(193),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(197),
    [sym_integer] = ACTIONS(191),
    [anon_sym_COMMA] = ACTIONS(695),
    [anon_sym_LPAREN] = ACTIONS(195),
    [anon_sym_let] = ACTIONS(407),
    [sym_identifier] = ACTIONS(189),
  },
  [483] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(265),
    [sym_type] = STATE(478),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(413),
    [anon_sym_LPAREN] = ACTIONS(415),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(417),
  },
  [484] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(770),
  },
  [485] = {
    [anon_sym_LPAREN_RPAREN] = ACTIONS(551),
    [anon_sym_in] = ACTIONS(691),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(691),
    [sym_integer] = ACTIONS(551),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_let] = ACTIONS(691),
    [sym_identifier] = ACTIONS(691),
  },
  [486] = {
    [sym_let_binding] = STATE(281),
    [sym_suspension] = STATE(281),
    [sym_atomic_expression] = STATE(281),
    [aux_sym_application_expression_repeat1] = STATE(354),
    [sym_record_expression] = STATE(281),
    [sym_expression] = STATE(355),
    [sym_unit] = STATE(277),
    [sym_application_expression] = STATE(281),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(453),
    [anon_sym_in] = ACTIONS(772),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(457),
    [sym_integer] = ACTIONS(451),
    [anon_sym_LPAREN] = ACTIONS(455),
    [anon_sym_let] = ACTIONS(459),
    [sym_identifier] = ACTIONS(449),
  },
  [487] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(286),
    [sym_type] = STATE(478),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(481),
    [anon_sym_LPAREN] = ACTIONS(483),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(485),
  },
  [488] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(774),
  },
  [489] = {
    [sym_record_type] = STATE(296),
    [sym_path] = STATE(297),
    [sym_type] = STATE(493),
    [sym_function_type] = STATE(296),
    [sym_unit_type] = STATE(296),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(496),
    [anon_sym_DOT] = ACTIONS(498),
    [anon_sym_LPAREN] = ACTIONS(500),
    [anon_sym_LBRACE] = ACTIONS(502),
    [sym_identifier] = ACTIONS(504),
  },
  [490] = {
    [sym__align] = ACTIONS(685),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(685),
  },
  [491] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(776),
  },
  [492] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(778),
  },
  [493] = {
    [sym__align] = ACTIONS(738),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(738),
  },
  [494] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(398),
    [sym_type] = STATE(478),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(629),
    [anon_sym_LPAREN] = ACTIONS(631),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(633),
  },
  [495] = {
    [sym_record_type] = STATE(162),
    [sym_path] = STATE(418),
    [sym_type] = STATE(478),
    [sym_function_type] = STATE(162),
    [sym_unit_type] = STATE(162),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(278),
    [anon_sym_DOT] = ACTIONS(657),
    [anon_sym_LPAREN] = ACTIONS(659),
    [anon_sym_LBRACE] = ACTIONS(284),
    [sym_identifier] = ACTIONS(661),
  },
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.count = 0, .reusable = false},
  [1] = {.count = 1, .reusable = false}, RECOVER(),
  [3] = {.count = 1, .reusable = true}, SHIFT_EXTRA(),
  [5] = {.count = 1, .reusable = true}, SHIFT(2),
  [7] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 0),
  [9] = {.count = 1, .reusable = true}, SHIFT(4),
  [11] = {.count = 1, .reusable = true}, SHIFT(3),
  [13] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 1),
  [15] = {.count = 1, .reusable = false}, SHIFT(11),
  [17] = {.count = 1, .reusable = true}, SHIFT(12),
  [19] = {.count = 1, .reusable = true}, SHIFT(13),
  [21] = {.count = 1, .reusable = true}, SHIFT(14),
  [23] = {.count = 1, .reusable = false}, SHIFT(15),
  [25] = {.count = 1, .reusable = false}, SHIFT(16),
  [27] = {.count = 1, .reusable = true}, SHIFT(23),
  [29] = {.count = 1, .reusable = true}, REDUCE(sym_definition, 1),
  [31] = {.count = 1, .reusable = true},  ACCEPT_INPUT(),
  [33] = {.count = 1, .reusable = true}, SHIFT(25),
  [35] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 2),
  [37] = {.count = 1, .reusable = true}, SHIFT(27),
  [39] = {.count = 1, .reusable = true}, REDUCE(sym_atomic_expression, 1),
  [41] = {.count = 1, .reusable = true}, REDUCE(sym_unit, 1),
  [43] = {.count = 1, .reusable = true}, SHIFT(29),
  [45] = {.count = 1, .reusable = true}, SHIFT(30),
  [47] = {.count = 1, .reusable = true}, SHIFT(31),
  [49] = {.count = 1, .reusable = true}, SHIFT(32),
  [51] = {.count = 1, .reusable = false}, SHIFT(33),
  [53] = {.count = 1, .reusable = false}, SHIFT(34),
  [55] = {.count = 1, .reusable = true}, SHIFT(39),
  [57] = {.count = 1, .reusable = true}, REDUCE(sym_path, 1),
  [59] = {.count = 1, .reusable = true}, REDUCE(sym_binding, 1),
  [61] = {.count = 1, .reusable = true}, REDUCE(sym_pattern, 1),
  [63] = {.count = 1, .reusable = true}, REDUCE(sym_atomic_pattern, 1),
  [65] = {.count = 1, .reusable = true}, REDUCE(sym_record_pattern, 1),
  [67] = {.count = 1, .reusable = true}, SHIFT(41),
  [69] = {.count = 1, .reusable = true}, SHIFT(43),
  [71] = {.count = 1, .reusable = true}, REDUCE(sym_application_pattern, 1),
  [73] = {.count = 1, .reusable = false}, SHIFT(45),
  [75] = {.count = 1, .reusable = true}, SHIFT(46),
  [77] = {.count = 1, .reusable = true}, SHIFT(47),
  [79] = {.count = 1, .reusable = false}, SHIFT(49),
  [81] = {.count = 1, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2),
  [83] = {.count = 2, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(53),
  [86] = {.count = 2, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(52),
  [89] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 3),
  [91] = {.count = 1, .reusable = true}, SHIFT(55),
  [93] = {.count = 1, .reusable = true}, REDUCE(sym_path, 2),
  [95] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 2),
  [97] = {.count = 1, .reusable = true}, SHIFT(58),
  [99] = {.count = 1, .reusable = true}, SHIFT(59),
  [101] = {.count = 1, .reusable = true}, SHIFT(61),
  [103] = {.count = 1, .reusable = true}, SHIFT(60),
  [105] = {.count = 1, .reusable = true}, SHIFT(64),
  [107] = {.count = 1, .reusable = true}, SHIFT(65),
  [109] = {.count = 1, .reusable = true}, SHIFT(67),
  [111] = {.count = 1, .reusable = true}, SHIFT(68),
  [113] = {.count = 1, .reusable = true}, SHIFT(70),
  [115] = {.count = 1, .reusable = true}, SHIFT(71),
  [117] = {.count = 1, .reusable = true}, SHIFT(72),
  [119] = {.count = 1, .reusable = false}, SHIFT(73),
  [121] = {.count = 1, .reusable = false}, SHIFT(74),
  [123] = {.count = 1, .reusable = true}, SHIFT(75),
  [125] = {.count = 1, .reusable = false}, SHIFT(81),
  [127] = {.count = 1, .reusable = true}, SHIFT(81),
  [129] = {.count = 1, .reusable = true}, SHIFT(82),
  [131] = {.count = 1, .reusable = false}, SHIFT(83),
  [133] = {.count = 1, .reusable = false}, SHIFT(80),
  [135] = {.count = 1, .reusable = false}, SHIFT(84),
  [137] = {.count = 1, .reusable = true}, REDUCE(sym_parameter_list, 1),
  [139] = {.count = 1, .reusable = true}, SHIFT(87),
  [141] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_pattern_repeat1, 2), SHIFT_REPEAT(14),
  [144] = {.count = 1, .reusable = true}, REDUCE(aux_sym_application_pattern_repeat1, 2),
  [146] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_pattern_repeat1, 2), SHIFT_REPEAT(27),
  [149] = {.count = 1, .reusable = true}, SHIFT(89),
  [151] = {.count = 1, .reusable = false}, SHIFT(88),
  [153] = {.count = 1, .reusable = true}, SHIFT(90),
  [155] = {.count = 1, .reusable = false}, SHIFT(91),
  [157] = {.count = 1, .reusable = true}, SHIFT(92),
  [159] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 3),
  [161] = {.count = 1, .reusable = false}, REDUCE(sym_parameter_list, 1),
  [163] = {.count = 1, .reusable = false}, SHIFT(96),
  [165] = {.count = 1, .reusable = true}, SHIFT(45),
  [167] = {.count = 1, .reusable = true}, SHIFT(97),
  [169] = {.count = 1, .reusable = true}, REDUCE(sym_type_definition_rhs, 1),
  [171] = {.count = 1, .reusable = false}, SHIFT(99),
  [173] = {.count = 1, .reusable = false}, SHIFT(100),
  [175] = {.count = 1, .reusable = true}, SHIFT(102),
  [177] = {.count = 1, .reusable = true}, SHIFT(104),
  [179] = {.count = 1, .reusable = true}, REDUCE(sym_path, 3),
  [181] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression_entry, 2),
  [183] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 3),
  [185] = {.count = 1, .reusable = true}, SHIFT(106),
  [187] = {.count = 1, .reusable = true}, SHIFT(108),
  [189] = {.count = 1, .reusable = false}, SHIFT(110),
  [191] = {.count = 1, .reusable = true}, SHIFT(110),
  [193] = {.count = 1, .reusable = true}, SHIFT(111),
  [195] = {.count = 1, .reusable = false}, SHIFT(112),
  [197] = {.count = 1, .reusable = false}, SHIFT(109),
  [199] = {.count = 1, .reusable = false}, SHIFT(113),
  [201] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2),
  [203] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2), SHIFT_REPEAT(31),
  [206] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2), SHIFT_REPEAT(118),
  [209] = {.count = 1, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2),
  [211] = {.count = 1, .reusable = true}, REDUCE(sym_unit_type, 1),
  [213] = {.count = 1, .reusable = true}, SHIFT(120),
  [215] = {.count = 1, .reusable = true}, SHIFT(121),
  [217] = {.count = 1, .reusable = true}, SHIFT(122),
  [219] = {.count = 1, .reusable = true}, SHIFT(124),
  [221] = {.count = 1, .reusable = true}, SHIFT(125),
  [223] = {.count = 1, .reusable = true}, SHIFT(128),
  [225] = {.count = 1, .reusable = true}, REDUCE(sym_type, 1),
  [227] = {.count = 1, .reusable = true}, SHIFT(130),
  [229] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_signature, 4),
  [231] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(38),
  [234] = {.count = 1, .reusable = false}, SHIFT(134),
  [236] = {.count = 1, .reusable = true}, SHIFT(132),
  [238] = {.count = 1, .reusable = false}, SHIFT(131),
  [240] = {.count = 1, .reusable = false}, SHIFT(133),
  [242] = {.count = 1, .reusable = false}, REDUCE(sym_atomic_expression, 1),
  [244] = {.count = 1, .reusable = false}, REDUCE(sym_unit, 1),
  [246] = {.count = 1, .reusable = false}, SHIFT(139),
  [248] = {.count = 1, .reusable = true}, REDUCE(sym_expression, 1),
  [250] = {.count = 1, .reusable = false}, REDUCE(sym_expression, 1),
  [252] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 4),
  [254] = {.count = 1, .reusable = true}, REDUCE(aux_sym_parameter_list_repeat1, 2),
  [256] = {.count = 2, .reusable = true}, REDUCE(aux_sym_parameter_list_repeat1, 2), SHIFT_REPEAT(87),
  [259] = {.count = 1, .reusable = true}, SHIFT(145),
  [261] = {.count = 1, .reusable = true}, SHIFT(146),
  [263] = {.count = 1, .reusable = true}, SHIFT(149),
  [265] = {.count = 1, .reusable = true}, SHIFT(152),
  [267] = {.count = 1, .reusable = true}, SHIFT(155),
  [269] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_signature, 4),
  [271] = {.count = 1, .reusable = true}, REDUCE(sym_type_alias_definition_rhs, 2),
  [273] = {.count = 1, .reusable = false}, REDUCE(aux_sym_parameter_list_repeat1, 2),
  [275] = {.count = 2, .reusable = false}, REDUCE(aux_sym_parameter_list_repeat1, 2), SHIFT_REPEAT(96),
  [278] = {.count = 1, .reusable = true}, SHIFT(157),
  [280] = {.count = 1, .reusable = true}, SHIFT(158),
  [282] = {.count = 1, .reusable = false}, SHIFT(159),
  [284] = {.count = 1, .reusable = false}, SHIFT(160),
  [286] = {.count = 1, .reusable = true}, SHIFT(161),
  [288] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 4),
  [290] = {.count = 1, .reusable = true}, SHIFT(166),
  [292] = {.count = 1, .reusable = true}, SHIFT(167),
  [294] = {.count = 1, .reusable = true}, SHIFT(169),
  [296] = {.count = 1, .reusable = true}, SHIFT(170),
  [298] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 5),
  [300] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 4),
  [302] = {.count = 1, .reusable = true}, SHIFT(173),
  [304] = {.count = 1, .reusable = true}, SHIFT(175),
  [306] = {.count = 1, .reusable = false}, SHIFT(174),
  [308] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression_entry, 3),
  [310] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(64),
  [313] = {.count = 1, .reusable = true}, SHIFT(185),
  [315] = {.count = 1, .reusable = true}, SHIFT(187),
  [317] = {.count = 1, .reusable = true}, SHIFT(38),
  [319] = {.count = 1, .reusable = true}, SHIFT(189),
  [321] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 2),
  [323] = {.count = 1, .reusable = true}, REDUCE(sym_record_label, 1),
  [325] = {.count = 1, .reusable = true}, SHIFT(190),
  [327] = {.count = 1, .reusable = true}, SHIFT(191),
  [329] = {.count = 1, .reusable = true}, SHIFT(194),
  [331] = {.count = 1, .reusable = true}, SHIFT(197),
  [333] = {.count = 1, .reusable = false}, REDUCE(sym_record_expression, 2),
  [335] = {.count = 1, .reusable = false}, REDUCE(sym_path, 1),
  [337] = {.count = 1, .reusable = true}, SHIFT(201),
  [339] = {.count = 1, .reusable = true}, SHIFT(204),
  [341] = {.count = 1, .reusable = true}, SHIFT(206),
  [343] = {.count = 1, .reusable = true}, SHIFT(209),
  [345] = {.count = 1, .reusable = true}, REDUCE(sym_application_expression, 2),
  [347] = {.count = 1, .reusable = false}, REDUCE(sym_application_expression, 2),
  [349] = {.count = 1, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 1),
  [351] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 3),
  [353] = {.count = 1, .reusable = true}, SHIFT(211),
  [355] = {.count = 1, .reusable = true}, SHIFT(212),
  [357] = {.count = 1, .reusable = true}, SHIFT(215),
  [359] = {.count = 1, .reusable = true}, SHIFT(216),
  [361] = {.count = 1, .reusable = true}, SHIFT(219),
  [363] = {.count = 1, .reusable = true}, SHIFT(222),
  [365] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 5),
  [367] = {.count = 1, .reusable = true}, SHIFT(224),
  [369] = {.count = 1, .reusable = true}, SHIFT(226),
  [371] = {.count = 1, .reusable = true}, SHIFT(230),
  [373] = {.count = 1, .reusable = true}, SHIFT(232),
  [375] = {.count = 1, .reusable = true}, SHIFT(234),
  [377] = {.count = 1, .reusable = false}, SHIFT(235),
  [379] = {.count = 1, .reusable = true}, SHIFT(236),
  [381] = {.count = 1, .reusable = false}, SHIFT(238),
  [383] = {.count = 1, .reusable = true}, SHIFT(240),
  [385] = {.count = 1, .reusable = false}, SHIFT(241),
  [387] = {.count = 1, .reusable = true}, SHIFT(242),
  [389] = {.count = 1, .reusable = true}, SHIFT(245),
  [391] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 6),
  [393] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 5),
  [395] = {.count = 1, .reusable = true}, SHIFT(246),
  [397] = {.count = 1, .reusable = true}, SHIFT(249),
  [399] = {.count = 1, .reusable = true}, SHIFT(250),
  [401] = {.count = 1, .reusable = true}, SHIFT(251),
  [403] = {.count = 1, .reusable = true}, SHIFT(252),
  [405] = {.count = 1, .reusable = true}, SHIFT(254),
  [407] = {.count = 1, .reusable = false}, SHIFT(255),
  [409] = {.count = 1, .reusable = true}, SHIFT(259),
  [411] = {.count = 1, .reusable = true}, SHIFT(260),
  [413] = {.count = 1, .reusable = true}, SHIFT(262),
  [415] = {.count = 1, .reusable = false}, SHIFT(263),
  [417] = {.count = 1, .reusable = true}, SHIFT(264),
  [419] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 3),
  [421] = {.count = 1, .reusable = true}, SHIFT(267),
  [423] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 2),
  [425] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 2), SHIFT_REPEAT(125),
  [428] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(128),
  [431] = {.count = 1, .reusable = true}, REDUCE(sym_function_type, 3),
  [433] = {.count = 1, .reusable = false}, REDUCE(sym_record_expression, 3),
  [435] = {.count = 1, .reusable = true}, SHIFT(269),
  [437] = {.count = 1, .reusable = true}, SHIFT(271),
  [439] = {.count = 1, .reusable = true}, REDUCE(sym_suspension, 3),
  [441] = {.count = 1, .reusable = false}, REDUCE(sym_suspension, 3),
  [443] = {.count = 1, .reusable = true}, SHIFT(274),
  [445] = {.count = 1, .reusable = true}, REDUCE(sym_expression, 3),
  [447] = {.count = 1, .reusable = false}, REDUCE(sym_expression, 3),
  [449] = {.count = 1, .reusable = false}, SHIFT(277),
  [451] = {.count = 1, .reusable = true}, SHIFT(277),
  [453] = {.count = 1, .reusable = true}, SHIFT(278),
  [455] = {.count = 1, .reusable = false}, SHIFT(279),
  [457] = {.count = 1, .reusable = false}, SHIFT(276),
  [459] = {.count = 1, .reusable = false}, SHIFT(280),
  [461] = {.count = 1, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2),
  [463] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(82),
  [466] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(80),
  [469] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(81),
  [472] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(83),
  [475] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(84),
  [478] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(81),
  [481] = {.count = 1, .reusable = true}, SHIFT(283),
  [483] = {.count = 1, .reusable = false}, SHIFT(284),
  [485] = {.count = 1, .reusable = true}, SHIFT(285),
  [487] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 4),
  [489] = {.count = 1, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat1, 2),
  [491] = {.count = 2, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat1, 2), SHIFT_REPEAT(288),
  [494] = {.count = 1, .reusable = true}, SHIFT(290),
  [496] = {.count = 1, .reusable = true}, SHIFT(291),
  [498] = {.count = 1, .reusable = true}, SHIFT(292),
  [500] = {.count = 1, .reusable = false}, SHIFT(293),
  [502] = {.count = 1, .reusable = false}, SHIFT(294),
  [504] = {.count = 1, .reusable = true}, SHIFT(295),
  [506] = {.count = 1, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat2, 2),
  [508] = {.count = 2, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat2, 2), SHIFT_REPEAT(299),
  [511] = {.count = 1, .reusable = true}, REDUCE(sym__semi, 1, .alias_sequence_id = 1),
  [513] = {.count = 1, .reusable = true}, SHIFT(303),
  [515] = {.count = 1, .reusable = true}, SHIFT(304),
  [517] = {.count = 1, .reusable = true}, SHIFT(305),
  [519] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 6),
  [521] = {.count = 1, .reusable = true}, SHIFT(310),
  [523] = {.count = 1, .reusable = true}, SHIFT(311),
  [525] = {.count = 1, .reusable = true}, SHIFT(314),
  [527] = {.count = 1, .reusable = true}, SHIFT(318),
  [529] = {.count = 1, .reusable = true}, SHIFT(321),
  [531] = {.count = 1, .reusable = true}, SHIFT(323),
  [533] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(111),
  [536] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(109),
  [539] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(110),
  [542] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(112),
  [545] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(113),
  [548] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(110),
  [551] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 6),
  [553] = {.count = 1, .reusable = true}, SHIFT(330),
  [555] = {.count = 1, .reusable = true}, SHIFT(332),
  [557] = {.count = 1, .reusable = true}, SHIFT(333),
  [559] = {.count = 1, .reusable = true}, SHIFT(336),
  [561] = {.count = 1, .reusable = true}, SHIFT(337),
  [563] = {.count = 1, .reusable = true}, SHIFT(338),
  [565] = {.count = 1, .reusable = true}, SHIFT(340),
  [567] = {.count = 1, .reusable = false}, REDUCE(sym_record_expression, 4),
  [569] = {.count = 1, .reusable = true}, SHIFT(341),
  [571] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(133),
  [574] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(139),
  [577] = {.count = 1, .reusable = true}, SHIFT(346),
  [579] = {.count = 1, .reusable = false}, SHIFT(345),
  [581] = {.count = 1, .reusable = false}, SHIFT(353),
  [583] = {.count = 1, .reusable = true}, SHIFT(356),
  [585] = {.count = 1, .reusable = true}, SHIFT(359),
  [587] = {.count = 1, .reusable = true}, REDUCE(sym_data_constructor_definition, 3),
  [589] = {.count = 1, .reusable = true}, SHIFT(360),
  [591] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 5),
  [593] = {.count = 1, .reusable = true}, SHIFT(361),
  [595] = {.count = 1, .reusable = true}, SHIFT(363),
  [597] = {.count = 1, .reusable = true}, SHIFT(366),
  [599] = {.count = 1, .reusable = true}, SHIFT(368),
  [601] = {.count = 1, .reusable = true}, SHIFT(369),
  [603] = {.count = 1, .reusable = true}, SHIFT(370),
  [605] = {.count = 1, .reusable = true}, SHIFT(373),
  [607] = {.count = 1, .reusable = true}, SHIFT(376),
  [609] = {.count = 1, .reusable = true}, SHIFT(378),
  [611] = {.count = 1, .reusable = true}, SHIFT(381),
  [613] = {.count = 1, .reusable = true}, SHIFT(383),
  [615] = {.count = 1, .reusable = false}, SHIFT(385),
  [617] = {.count = 1, .reusable = true}, SHIFT(386),
  [619] = {.count = 1, .reusable = true}, SHIFT(390),
  [621] = {.count = 1, .reusable = true}, SHIFT(392),
  [623] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 5),
  [625] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 4),
  [627] = {.count = 1, .reusable = true}, SHIFT(394),
  [629] = {.count = 1, .reusable = true}, SHIFT(395),
  [631] = {.count = 1, .reusable = false}, SHIFT(396),
  [633] = {.count = 1, .reusable = true}, SHIFT(397),
  [635] = {.count = 1, .reusable = false}, REDUCE(sym_record_expression, 5),
  [637] = {.count = 1, .reusable = false}, SHIFT(400),
  [639] = {.count = 1, .reusable = true}, SHIFT(401),
  [641] = {.count = 1, .reusable = false}, SHIFT(402),
  [643] = {.count = 1, .reusable = true}, SHIFT(403),
  [645] = {.count = 1, .reusable = true}, SHIFT(406),
  [647] = {.count = 1, .reusable = true}, SHIFT(407),
  [649] = {.count = 1, .reusable = true}, SHIFT(408),
  [651] = {.count = 1, .reusable = true}, SHIFT(409),
  [653] = {.count = 1, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 1),
  [655] = {.count = 1, .reusable = true}, SHIFT(413),
  [657] = {.count = 1, .reusable = true}, SHIFT(415),
  [659] = {.count = 1, .reusable = false}, SHIFT(416),
  [661] = {.count = 1, .reusable = true}, SHIFT(417),
  [663] = {.count = 1, .reusable = true}, SHIFT(420),
  [665] = {.count = 1, .reusable = true}, SHIFT(421),
  [667] = {.count = 1, .reusable = true}, SHIFT(422),
  [669] = {.count = 1, .reusable = true}, SHIFT(424),
  [671] = {.count = 1, .reusable = true}, SHIFT(428),
  [673] = {.count = 1, .reusable = true}, SHIFT(429),
  [675] = {.count = 1, .reusable = true}, SHIFT(430),
  [677] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(238),
  [680] = {.count = 1, .reusable = true}, SHIFT(435),
  [682] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(255),
  [685] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 6),
  [687] = {.count = 1, .reusable = true}, SHIFT(440),
  [689] = {.count = 1, .reusable = true}, SHIFT(443),
  [691] = {.count = 1, .reusable = false}, REDUCE(sym_record_expression, 6),
  [693] = {.count = 1, .reusable = true}, SHIFT(446),
  [695] = {.count = 1, .reusable = true}, REDUCE(sym_let_binding, 6),
  [697] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(278),
  [700] = {.count = 1, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2),
  [702] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(276),
  [705] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(277),
  [708] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(279),
  [711] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(280),
  [714] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(277),
  [717] = {.count = 1, .reusable = true}, SHIFT(451),
  [719] = {.count = 1, .reusable = true}, SHIFT(454),
  [721] = {.count = 1, .reusable = true}, SHIFT(457),
  [723] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(366),
  [726] = {.count = 1, .reusable = true}, SHIFT(458),
  [728] = {.count = 1, .reusable = true}, SHIFT(459),
  [730] = {.count = 1, .reusable = true}, SHIFT(460),
  [732] = {.count = 1, .reusable = false}, SHIFT(461),
  [734] = {.count = 1, .reusable = true}, SHIFT(462),
  [736] = {.count = 1, .reusable = false}, SHIFT(463),
  [738] = {.count = 1, .reusable = true}, REDUCE(sym_function_type, 7),
  [740] = {.count = 1, .reusable = true}, SHIFT(464),
  [742] = {.count = 1, .reusable = true}, SHIFT(466),
  [744] = {.count = 1, .reusable = true}, SHIFT(468),
  [746] = {.count = 1, .reusable = false}, SHIFT(470),
  [748] = {.count = 1, .reusable = true}, SHIFT(471),
  [750] = {.count = 1, .reusable = true}, SHIFT(473),
  [752] = {.count = 1, .reusable = true}, SHIFT(475),
  [754] = {.count = 1, .reusable = true}, SHIFT(476),
  [756] = {.count = 1, .reusable = true}, SHIFT(479),
  [758] = {.count = 1, .reusable = true}, SHIFT(481),
  [760] = {.count = 1, .reusable = true}, SHIFT(483),
  [762] = {.count = 1, .reusable = true}, SHIFT(485),
  [764] = {.count = 1, .reusable = true}, SHIFT(487),
  [766] = {.count = 1, .reusable = true}, SHIFT(489),
  [768] = {.count = 1, .reusable = true}, SHIFT(490),
  [770] = {.count = 1, .reusable = true}, SHIFT(491),
  [772] = {.count = 1, .reusable = false}, REDUCE(sym_let_binding, 6),
  [774] = {.count = 1, .reusable = true}, SHIFT(492),
  [776] = {.count = 1, .reusable = true}, SHIFT(494),
  [778] = {.count = 1, .reusable = true}, SHIFT(495),
};

void *tree_sitter_miu_external_scanner_create();
void tree_sitter_miu_external_scanner_destroy(void *);
bool tree_sitter_miu_external_scanner_scan(void *, TSLexer *, const bool *);
unsigned tree_sitter_miu_external_scanner_serialize(void *, char *);
void tree_sitter_miu_external_scanner_deserialize(void *, const char *, unsigned);

#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_miu() {
  static TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .symbol_metadata = ts_symbol_metadata,
    .parse_table = (const unsigned short *)ts_parse_table,
    .parse_actions = ts_parse_actions,
    .lex_modes = ts_lex_modes,
    .symbol_names = ts_symbol_names,
    .alias_sequences = (const TSSymbol *)ts_alias_sequences,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .lex_fn = ts_lex,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .external_scanner = {
      (const bool *)ts_external_scanner_states,
      ts_external_scanner_symbol_map,
      tree_sitter_miu_external_scanner_create,
      tree_sitter_miu_external_scanner_destroy,
      tree_sitter_miu_external_scanner_scan,
      tree_sitter_miu_external_scanner_serialize,
      tree_sitter_miu_external_scanner_deserialize,
    },
  };
  return &language;
}
