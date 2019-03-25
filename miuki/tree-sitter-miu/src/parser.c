#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 9
#define STATE_COUNT 512
#define SYMBOL_COUNT 66
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
  aux_sym_record_expression_repeat1 = 63,
  aux_sym_data_type_definition_rhs_repeat1 = 64,
  aux_sym_data_type_definition_rhs_repeat2 = 65,
  anon_alias_sym__semi = 66,
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
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(57);
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
        SKIP(56);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 57:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(56);
      if (lookahead == '\r')
        SKIP(58);
      END_STATE();
    case 58:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(56);
      END_STATE();
    case 59:
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(60);
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
        SKIP(59);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 60:
      if (lookahead == '\n')
        SKIP(59);
      if (lookahead == '\r')
        SKIP(61);
      END_STATE();
    case 61:
      if (lookahead == '\n')
        SKIP(59);
      END_STATE();
    case 62:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(63);
      if (lookahead == 'w')
        ADVANCE(64);
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
        SKIP(62);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 63:
      if (lookahead == '\n')
        SKIP(62);
      if (lookahead == '\r')
        SKIP(65);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i')
        ADVANCE(66);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 65:
      if (lookahead == '\n')
        SKIP(62);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't')
        ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h')
        ADVANCE(68);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_with);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 69:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(70);
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
      if (lookahead == '\n')
        SKIP(69);
      if (lookahead == '\r')
        SKIP(71);
      END_STATE();
    case 71:
      if (lookahead == '\n')
        SKIP(69);
      END_STATE();
    case 72:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == ':')
        ADVANCE(7);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(73);
      if (lookahead == 'w')
        ADVANCE(74);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(72);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 73:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(72);
      if (lookahead == '\r')
        SKIP(75);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h')
        ADVANCE(76);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 75:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(72);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(77);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r')
        ADVANCE(78);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(79);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_where);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
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
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(85);
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
        SKIP(84);
      END_STATE();
    case 85:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(84);
      if (lookahead == '\r')
        SKIP(86);
      END_STATE();
    case 86:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(84);
      END_STATE();
    case 87:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(88);
      if (lookahead == 'l')
        ADVANCE(89);
      if (lookahead == '{')
        ADVANCE(16);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(87);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 88:
      if (lookahead == '\n')
        SKIP(87);
      if (lookahead == '\r')
        SKIP(90);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e')
        ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 90:
      if (lookahead == '\n')
        SKIP(87);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't')
        ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 93:
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
        SKIP(94);
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
        SKIP(93);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 94:
      if (lookahead == '\n')
        SKIP(93);
      if (lookahead == '\r')
        SKIP(95);
      END_STATE();
    case 95:
      if (lookahead == '\n')
        SKIP(93);
      END_STATE();
    case 96:
      if (lookahead == '(')
        ADVANCE(2);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(97);
      if (lookahead == 'l')
        ADVANCE(89);
      if (lookahead == 'w')
        ADVANCE(64);
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
        SKIP(96);
      if (('0' <= lookahead && lookahead <= '9'))
        ADVANCE(19);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 97:
      if (lookahead == '\n')
        SKIP(96);
      if (lookahead == '\r')
        SKIP(98);
      END_STATE();
    case 98:
      if (lookahead == '\n')
        SKIP(96);
      END_STATE();
    case 99:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(100);
      if (lookahead == 'w')
        ADVANCE(101);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(99);
      END_STATE();
    case 100:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(99);
      if (lookahead == '\r')
        SKIP(102);
      END_STATE();
    case 101:
      if (lookahead == 'h')
        ADVANCE(28);
      END_STATE();
    case 102:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(99);
      END_STATE();
    case 103:
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(104);
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
        SKIP(103);
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
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(107);
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
        SKIP(106);
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
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(110);
      if (lookahead == 'i')
        ADVANCE(11);
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
        SKIP(109);
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
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(113);
      if (lookahead == 'w')
        ADVANCE(64);
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
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
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
      if (lookahead == ')')
        ADVANCE(3);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(116);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(115);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
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
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == ';')
        ADVANCE(8);
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
      if (lookahead == ',')
        ADVANCE(4);
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == ';')
        ADVANCE(8);
      if (lookahead == '=')
        ADVANCE(9);
      if (lookahead == '\\')
        SKIP(122);
      if (lookahead == 'w')
        ADVANCE(101);
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
        SKIP(121);
      END_STATE();
    case 122:
      if (lookahead == '\n')
        SKIP(121);
      if (lookahead == '\r')
        SKIP(123);
      END_STATE();
    case 123:
      if (lookahead == '\n')
        SKIP(121);
      END_STATE();
    case 124:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '-')
        ADVANCE(5);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(125);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(124);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 125:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(124);
      if (lookahead == '\r')
        SKIP(126);
      END_STATE();
    case 126:
      if (lookahead == 0)
        ADVANCE(1);
      if (lookahead == '\n')
        SKIP(124);
      END_STATE();
    case 127:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '\\')
        SKIP(128);
      if (lookahead == 'i')
        ADVANCE(11);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(127);
      END_STATE();
    case 128:
      if (lookahead == '\n')
        SKIP(127);
      if (lookahead == '\r')
        SKIP(129);
      END_STATE();
    case 129:
      if (lookahead == '\n')
        SKIP(127);
      END_STATE();
    case 130:
      if (lookahead == '-')
        ADVANCE(44);
      if (lookahead == '.')
        ADVANCE(6);
      if (lookahead == '\\')
        SKIP(131);
      if (lookahead == 'i')
        ADVANCE(132);
      if (lookahead == '{')
        ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ' ||
          lookahead == 8203 ||
          lookahead == 8288 ||
          lookahead == 65279)
        SKIP(130);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 131:
      if (lookahead == '\n')
        SKIP(130);
      if (lookahead == '\r')
        SKIP(133);
      END_STATE();
    case 132:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n')
        ADVANCE(134);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z'))
        ADVANCE(52);
      END_STATE();
    case 133:
      if (lookahead == '\n')
        SKIP(130);
      END_STATE();
    case 134:
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
  [12] = {.lex_state = 59},
  [13] = {.lex_state = 59},
  [14] = {.lex_state = 56},
  [15] = {.lex_state = 62},
  [16] = {.lex_state = 69},
  [17] = {.lex_state = 69},
  [18] = {.lex_state = 69},
  [19] = {.lex_state = 69},
  [20] = {.lex_state = 69},
  [21] = {.lex_state = 69},
  [22] = {.lex_state = 69},
  [23] = {.lex_state = 69},
  [24] = {.lex_state = 72},
  [25] = {.lex_state = 43},
  [26] = {.lex_state = 43},
  [27] = {.lex_state = 0, .external_lex_state = 2},
  [28] = {.lex_state = 69},
  [29] = {.lex_state = 69},
  [30] = {.lex_state = 69},
  [31] = {.lex_state = 59},
  [32] = {.lex_state = 56},
  [33] = {.lex_state = 56},
  [34] = {.lex_state = 56},
  [35] = {.lex_state = 80},
  [36] = {.lex_state = 56},
  [37] = {.lex_state = 84},
  [38] = {.lex_state = 80},
  [39] = {.lex_state = 56},
  [40] = {.lex_state = 56},
  [41] = {.lex_state = 69},
  [42] = {.lex_state = 69},
  [43] = {.lex_state = 87},
  [44] = {.lex_state = 69},
  [45] = {.lex_state = 69},
  [46] = {.lex_state = 48, .external_lex_state = 3},
  [47] = {.lex_state = 56},
  [48] = {.lex_state = 56},
  [49] = {.lex_state = 0, .external_lex_state = 2},
  [50] = {.lex_state = 72},
  [51] = {.lex_state = 72},
  [52] = {.lex_state = 0, .external_lex_state = 2},
  [53] = {.lex_state = 48},
  [54] = {.lex_state = 56},
  [55] = {.lex_state = 0, .external_lex_state = 2},
  [56] = {.lex_state = 87},
  [57] = {.lex_state = 69},
  [58] = {.lex_state = 69},
  [59] = {.lex_state = 84},
  [60] = {.lex_state = 80},
  [61] = {.lex_state = 69},
  [62] = {.lex_state = 59},
  [63] = {.lex_state = 56},
  [64] = {.lex_state = 84},
  [65] = {.lex_state = 56},
  [66] = {.lex_state = 87},
  [67] = {.lex_state = 80},
  [68] = {.lex_state = 56},
  [69] = {.lex_state = 56},
  [70] = {.lex_state = 56},
  [71] = {.lex_state = 93},
  [72] = {.lex_state = 72, .external_lex_state = 2},
  [73] = {.lex_state = 56},
  [74] = {.lex_state = 56},
  [75] = {.lex_state = 56},
  [76] = {.lex_state = 84, .external_lex_state = 2},
  [77] = {.lex_state = 72, .external_lex_state = 2},
  [78] = {.lex_state = 84, .external_lex_state = 2},
  [79] = {.lex_state = 0, .external_lex_state = 2},
  [80] = {.lex_state = 69},
  [81] = {.lex_state = 96},
  [82] = {.lex_state = 0, .external_lex_state = 2},
  [83] = {.lex_state = 0, .external_lex_state = 2},
  [84] = {.lex_state = 56},
  [85] = {.lex_state = 87},
  [86] = {.lex_state = 56},
  [87] = {.lex_state = 56, .external_lex_state = 2},
  [88] = {.lex_state = 0, .external_lex_state = 2},
  [89] = {.lex_state = 56, .external_lex_state = 2},
  [90] = {.lex_state = 0, .external_lex_state = 2},
  [91] = {.lex_state = 69},
  [92] = {.lex_state = 56},
  [93] = {.lex_state = 56, .external_lex_state = 4},
  [94] = {.lex_state = 56},
  [95] = {.lex_state = 56},
  [96] = {.lex_state = 99, .external_lex_state = 2},
  [97] = {.lex_state = 99, .external_lex_state = 2},
  [98] = {.lex_state = 72, .external_lex_state = 2},
  [99] = {.lex_state = 0, .external_lex_state = 2},
  [100] = {.lex_state = 72},
  [101] = {.lex_state = 56},
  [102] = {.lex_state = 0, .external_lex_state = 2},
  [103] = {.lex_state = 56},
  [104] = {.lex_state = 69},
  [105] = {.lex_state = 69},
  [106] = {.lex_state = 72},
  [107] = {.lex_state = 0, .external_lex_state = 2},
  [108] = {.lex_state = 87},
  [109] = {.lex_state = 80},
  [110] = {.lex_state = 59},
  [111] = {.lex_state = 84},
  [112] = {.lex_state = 103},
  [113] = {.lex_state = 96},
  [114] = {.lex_state = 56},
  [115] = {.lex_state = 87},
  [116] = {.lex_state = 56},
  [117] = {.lex_state = 106},
  [118] = {.lex_state = 109},
  [119] = {.lex_state = 106},
  [120] = {.lex_state = 84},
  [121] = {.lex_state = 80},
  [122] = {.lex_state = 69},
  [123] = {.lex_state = 84},
  [124] = {.lex_state = 56},
  [125] = {.lex_state = 84, .external_lex_state = 2},
  [126] = {.lex_state = 56},
  [127] = {.lex_state = 69},
  [128] = {.lex_state = 69},
  [129] = {.lex_state = 72, .external_lex_state = 2},
  [130] = {.lex_state = 69},
  [131] = {.lex_state = 69},
  [132] = {.lex_state = 56},
  [133] = {.lex_state = 56},
  [134] = {.lex_state = 84, .external_lex_state = 2},
  [135] = {.lex_state = 56},
  [136] = {.lex_state = 56},
  [137] = {.lex_state = 0, .external_lex_state = 2},
  [138] = {.lex_state = 56},
  [139] = {.lex_state = 56},
  [140] = {.lex_state = 112},
  [141] = {.lex_state = 56},
  [142] = {.lex_state = 62},
  [143] = {.lex_state = 84},
  [144] = {.lex_state = 56},
  [145] = {.lex_state = 56},
  [146] = {.lex_state = 56, .external_lex_state = 2},
  [147] = {.lex_state = 56},
  [148] = {.lex_state = 56},
  [149] = {.lex_state = 115},
  [150] = {.lex_state = 115},
  [151] = {.lex_state = 115},
  [152] = {.lex_state = 69},
  [153] = {.lex_state = 69},
  [154] = {.lex_state = 56, .external_lex_state = 2},
  [155] = {.lex_state = 56, .external_lex_state = 2},
  [156] = {.lex_state = 0, .external_lex_state = 2},
  [157] = {.lex_state = 69},
  [158] = {.lex_state = 56},
  [159] = {.lex_state = 118},
  [160] = {.lex_state = 69},
  [161] = {.lex_state = 56, .external_lex_state = 4},
  [162] = {.lex_state = 0, .external_lex_state = 5},
  [163] = {.lex_state = 99, .external_lex_state = 2},
  [164] = {.lex_state = 69},
  [165] = {.lex_state = 56},
  [166] = {.lex_state = 99, .external_lex_state = 2},
  [167] = {.lex_state = 56},
  [168] = {.lex_state = 0, .external_lex_state = 2},
  [169] = {.lex_state = 121},
  [170] = {.lex_state = 56},
  [171] = {.lex_state = 56},
  [172] = {.lex_state = 56},
  [173] = {.lex_state = 99},
  [174] = {.lex_state = 121},
  [175] = {.lex_state = 99},
  [176] = {.lex_state = 72},
  [177] = {.lex_state = 69},
  [178] = {.lex_state = 56},
  [179] = {.lex_state = 87},
  [180] = {.lex_state = 69},
  [181] = {.lex_state = 56},
  [182] = {.lex_state = 56},
  [183] = {.lex_state = 72},
  [184] = {.lex_state = 0, .external_lex_state = 2},
  [185] = {.lex_state = 59},
  [186] = {.lex_state = 62},
  [187] = {.lex_state = 56},
  [188] = {.lex_state = 106},
  [189] = {.lex_state = 115},
  [190] = {.lex_state = 69},
  [191] = {.lex_state = 106},
  [192] = {.lex_state = 106},
  [193] = {.lex_state = 87},
  [194] = {.lex_state = 84},
  [195] = {.lex_state = 84, .external_lex_state = 2},
  [196] = {.lex_state = 69},
  [197] = {.lex_state = 69},
  [198] = {.lex_state = 56},
  [199] = {.lex_state = 56},
  [200] = {.lex_state = 72, .external_lex_state = 2},
  [201] = {.lex_state = 69},
  [202] = {.lex_state = 56},
  [203] = {.lex_state = 124, .external_lex_state = 2},
  [204] = {.lex_state = 84, .external_lex_state = 2},
  [205] = {.lex_state = 72, .external_lex_state = 2},
  [206] = {.lex_state = 0, .external_lex_state = 2},
  [207] = {.lex_state = 56},
  [208] = {.lex_state = 84},
  [209] = {.lex_state = 62},
  [210] = {.lex_state = 69},
  [211] = {.lex_state = 56},
  [212] = {.lex_state = 62},
  [213] = {.lex_state = 56},
  [214] = {.lex_state = 56},
  [215] = {.lex_state = 56},
  [216] = {.lex_state = 56},
  [217] = {.lex_state = 0, .external_lex_state = 2},
  [218] = {.lex_state = 56, .external_lex_state = 2},
  [219] = {.lex_state = 115},
  [220] = {.lex_state = 69},
  [221] = {.lex_state = 115},
  [222] = {.lex_state = 115},
  [223] = {.lex_state = 0, .external_lex_state = 2},
  [224] = {.lex_state = 87},
  [225] = {.lex_state = 56, .external_lex_state = 2},
  [226] = {.lex_state = 56},
  [227] = {.lex_state = 0, .external_lex_state = 2},
  [228] = {.lex_state = 56},
  [229] = {.lex_state = 118},
  [230] = {.lex_state = 56},
  [231] = {.lex_state = 56},
  [232] = {.lex_state = 56, .external_lex_state = 4},
  [233] = {.lex_state = 0, .external_lex_state = 5},
  [234] = {.lex_state = 56, .external_lex_state = 4},
  [235] = {.lex_state = 56, .external_lex_state = 4},
  [236] = {.lex_state = 99, .external_lex_state = 2},
  [237] = {.lex_state = 56},
  [238] = {.lex_state = 99, .external_lex_state = 2},
  [239] = {.lex_state = 99, .external_lex_state = 2},
  [240] = {.lex_state = 99},
  [241] = {.lex_state = 69},
  [242] = {.lex_state = 121},
  [243] = {.lex_state = 69},
  [244] = {.lex_state = 56},
  [245] = {.lex_state = 99},
  [246] = {.lex_state = 56},
  [247] = {.lex_state = 0, .external_lex_state = 2},
  [248] = {.lex_state = 87},
  [249] = {.lex_state = 69},
  [250] = {.lex_state = 56},
  [251] = {.lex_state = 56},
  [252] = {.lex_state = 84, .external_lex_state = 2},
  [253] = {.lex_state = 84, .external_lex_state = 2},
  [254] = {.lex_state = 56},
  [255] = {.lex_state = 56},
  [256] = {.lex_state = 56, .external_lex_state = 2},
  [257] = {.lex_state = 56, .external_lex_state = 2},
  [258] = {.lex_state = 56},
  [259] = {.lex_state = 56},
  [260] = {.lex_state = 99, .external_lex_state = 2},
  [261] = {.lex_state = 99, .external_lex_state = 2},
  [262] = {.lex_state = 72, .external_lex_state = 2},
  [263] = {.lex_state = 56},
  [264] = {.lex_state = 109},
  [265] = {.lex_state = 106},
  [266] = {.lex_state = 109},
  [267] = {.lex_state = 87},
  [268] = {.lex_state = 106},
  [269] = {.lex_state = 56},
  [270] = {.lex_state = 56},
  [271] = {.lex_state = 106},
  [272] = {.lex_state = 106},
  [273] = {.lex_state = 59},
  [274] = {.lex_state = 69},
  [275] = {.lex_state = 69},
  [276] = {.lex_state = 56},
  [277] = {.lex_state = 115},
  [278] = {.lex_state = 115},
  [279] = {.lex_state = 56},
  [280] = {.lex_state = 56},
  [281] = {.lex_state = 84},
  [282] = {.lex_state = 84},
  [283] = {.lex_state = 84},
  [284] = {.lex_state = 56},
  [285] = {.lex_state = 69},
  [286] = {.lex_state = 0, .external_lex_state = 2},
  [287] = {.lex_state = 84},
  [288] = {.lex_state = 62},
  [289] = {.lex_state = 87},
  [290] = {.lex_state = 62},
  [291] = {.lex_state = 62},
  [292] = {.lex_state = 56},
  [293] = {.lex_state = 56},
  [294] = {.lex_state = 56},
  [295] = {.lex_state = 115},
  [296] = {.lex_state = 87},
  [297] = {.lex_state = 115},
  [298] = {.lex_state = 96},
  [299] = {.lex_state = 127},
  [300] = {.lex_state = 127},
  [301] = {.lex_state = 56},
  [302] = {.lex_state = 56},
  [303] = {.lex_state = 130},
  [304] = {.lex_state = 130},
  [305] = {.lex_state = 127},
  [306] = {.lex_state = 56},
  [307] = {.lex_state = 56},
  [308] = {.lex_state = 118},
  [309] = {.lex_state = 118},
  [310] = {.lex_state = 118},
  [311] = {.lex_state = 69},
  [312] = {.lex_state = 118},
  [313] = {.lex_state = 0, .external_lex_state = 2},
  [314] = {.lex_state = 0, .external_lex_state = 5},
  [315] = {.lex_state = 56},
  [316] = {.lex_state = 56},
  [317] = {.lex_state = 56},
  [318] = {.lex_state = 84, .external_lex_state = 5},
  [319] = {.lex_state = 0, .external_lex_state = 5},
  [320] = {.lex_state = 84, .external_lex_state = 5},
  [321] = {.lex_state = 0, .external_lex_state = 5},
  [322] = {.lex_state = 69},
  [323] = {.lex_state = 0, .external_lex_state = 2},
  [324] = {.lex_state = 115},
  [325] = {.lex_state = 99},
  [326] = {.lex_state = 56},
  [327] = {.lex_state = 56},
  [328] = {.lex_state = 121},
  [329] = {.lex_state = 69},
  [330] = {.lex_state = 99},
  [331] = {.lex_state = 121},
  [332] = {.lex_state = 87},
  [333] = {.lex_state = 84, .external_lex_state = 2},
  [334] = {.lex_state = 69},
  [335] = {.lex_state = 84, .external_lex_state = 2},
  [336] = {.lex_state = 56},
  [337] = {.lex_state = 56, .external_lex_state = 2},
  [338] = {.lex_state = 69},
  [339] = {.lex_state = 56, .external_lex_state = 2},
  [340] = {.lex_state = 56, .external_lex_state = 2},
  [341] = {.lex_state = 99, .external_lex_state = 2},
  [342] = {.lex_state = 69},
  [343] = {.lex_state = 99, .external_lex_state = 2},
  [344] = {.lex_state = 56},
  [345] = {.lex_state = 72},
  [346] = {.lex_state = 127},
  [347] = {.lex_state = 106},
  [348] = {.lex_state = 69},
  [349] = {.lex_state = 106},
  [350] = {.lex_state = 106},
  [351] = {.lex_state = 115},
  [352] = {.lex_state = 115},
  [353] = {.lex_state = 84},
  [354] = {.lex_state = 84},
  [355] = {.lex_state = 69},
  [356] = {.lex_state = 84},
  [357] = {.lex_state = 56},
  [358] = {.lex_state = 72, .external_lex_state = 2},
  [359] = {.lex_state = 56},
  [360] = {.lex_state = 84},
  [361] = {.lex_state = 56},
  [362] = {.lex_state = 0, .external_lex_state = 2},
  [363] = {.lex_state = 127},
  [364] = {.lex_state = 56},
  [365] = {.lex_state = 56},
  [366] = {.lex_state = 84},
  [367] = {.lex_state = 127},
  [368] = {.lex_state = 56},
  [369] = {.lex_state = 127},
  [370] = {.lex_state = 62},
  [371] = {.lex_state = 84},
  [372] = {.lex_state = 56},
  [373] = {.lex_state = 130},
  [374] = {.lex_state = 69},
  [375] = {.lex_state = 56},
  [376] = {.lex_state = 130},
  [377] = {.lex_state = 130},
  [378] = {.lex_state = 87},
  [379] = {.lex_state = 118},
  [380] = {.lex_state = 69},
  [381] = {.lex_state = 118},
  [382] = {.lex_state = 56},
  [383] = {.lex_state = 56},
  [384] = {.lex_state = 84, .external_lex_state = 5},
  [385] = {.lex_state = 69},
  [386] = {.lex_state = 0, .external_lex_state = 5},
  [387] = {.lex_state = 69},
  [388] = {.lex_state = 56},
  [389] = {.lex_state = 56},
  [390] = {.lex_state = 84, .external_lex_state = 5},
  [391] = {.lex_state = 56},
  [392] = {.lex_state = 56},
  [393] = {.lex_state = 84},
  [394] = {.lex_state = 115},
  [395] = {.lex_state = 84},
  [396] = {.lex_state = 56},
  [397] = {.lex_state = 84, .external_lex_state = 2},
  [398] = {.lex_state = 56},
  [399] = {.lex_state = 84, .external_lex_state = 2},
  [400] = {.lex_state = 56, .external_lex_state = 2},
  [401] = {.lex_state = 87},
  [402] = {.lex_state = 56, .external_lex_state = 2},
  [403] = {.lex_state = 99, .external_lex_state = 2},
  [404] = {.lex_state = 56},
  [405] = {.lex_state = 99, .external_lex_state = 2},
  [406] = {.lex_state = 87},
  [407] = {.lex_state = 106},
  [408] = {.lex_state = 87},
  [409] = {.lex_state = 106},
  [410] = {.lex_state = 115},
  [411] = {.lex_state = 115},
  [412] = {.lex_state = 56},
  [413] = {.lex_state = 84},
  [414] = {.lex_state = 56},
  [415] = {.lex_state = 84},
  [416] = {.lex_state = 72, .external_lex_state = 2},
  [417] = {.lex_state = 56},
  [418] = {.lex_state = 56},
  [419] = {.lex_state = 84},
  [420] = {.lex_state = 84},
  [421] = {.lex_state = 84},
  [422] = {.lex_state = 87},
  [423] = {.lex_state = 0, .external_lex_state = 2},
  [424] = {.lex_state = 87},
  [425] = {.lex_state = 127},
  [426] = {.lex_state = 56},
  [427] = {.lex_state = 84},
  [428] = {.lex_state = 56},
  [429] = {.lex_state = 130},
  [430] = {.lex_state = 87},
  [431] = {.lex_state = 130},
  [432] = {.lex_state = 130},
  [433] = {.lex_state = 0, .external_lex_state = 2},
  [434] = {.lex_state = 118},
  [435] = {.lex_state = 56},
  [436] = {.lex_state = 118},
  [437] = {.lex_state = 56},
  [438] = {.lex_state = 56},
  [439] = {.lex_state = 118},
  [440] = {.lex_state = 118},
  [441] = {.lex_state = 84, .external_lex_state = 5},
  [442] = {.lex_state = 56},
  [443] = {.lex_state = 56},
  [444] = {.lex_state = 0, .external_lex_state = 5},
  [445] = {.lex_state = 69},
  [446] = {.lex_state = 84, .external_lex_state = 5},
  [447] = {.lex_state = 84, .external_lex_state = 5},
  [448] = {.lex_state = 0, .external_lex_state = 5},
  [449] = {.lex_state = 0, .external_lex_state = 2},
  [450] = {.lex_state = 56},
  [451] = {.lex_state = 84},
  [452] = {.lex_state = 121},
  [453] = {.lex_state = 84},
  [454] = {.lex_state = 115},
  [455] = {.lex_state = 127},
  [456] = {.lex_state = 115},
  [457] = {.lex_state = 109},
  [458] = {.lex_state = 127},
  [459] = {.lex_state = 72, .external_lex_state = 2},
  [460] = {.lex_state = 115},
  [461] = {.lex_state = 84},
  [462] = {.lex_state = 69},
  [463] = {.lex_state = 84},
  [464] = {.lex_state = 56},
  [465] = {.lex_state = 127},
  [466] = {.lex_state = 84},
  [467] = {.lex_state = 56},
  [468] = {.lex_state = 127},
  [469] = {.lex_state = 115},
  [470] = {.lex_state = 118},
  [471] = {.lex_state = 69},
  [472] = {.lex_state = 118},
  [473] = {.lex_state = 56},
  [474] = {.lex_state = 115},
  [475] = {.lex_state = 84},
  [476] = {.lex_state = 56},
  [477] = {.lex_state = 56},
  [478] = {.lex_state = 121},
  [479] = {.lex_state = 84},
  [480] = {.lex_state = 87},
  [481] = {.lex_state = 84},
  [482] = {.lex_state = 87},
  [483] = {.lex_state = 84},
  [484] = {.lex_state = 84},
  [485] = {.lex_state = 56},
  [486] = {.lex_state = 84},
  [487] = {.lex_state = 127},
  [488] = {.lex_state = 84},
  [489] = {.lex_state = 87},
  [490] = {.lex_state = 84},
  [491] = {.lex_state = 118},
  [492] = {.lex_state = 56},
  [493] = {.lex_state = 118},
  [494] = {.lex_state = 84},
  [495] = {.lex_state = 0, .external_lex_state = 5},
  [496] = {.lex_state = 84},
  [497] = {.lex_state = 121},
  [498] = {.lex_state = 56},
  [499] = {.lex_state = 56},
  [500] = {.lex_state = 56},
  [501] = {.lex_state = 115},
  [502] = {.lex_state = 127},
  [503] = {.lex_state = 56},
  [504] = {.lex_state = 115},
  [505] = {.lex_state = 56},
  [506] = {.lex_state = 0, .external_lex_state = 5},
  [507] = {.lex_state = 84},
  [508] = {.lex_state = 84},
  [509] = {.lex_state = 0, .external_lex_state = 5},
  [510] = {.lex_state = 56},
  [511] = {.lex_state = 56},
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
    [aux_sym_application_expression_repeat1] = STATE(20),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(20),
    [sym_binding] = STATE(22),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
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
    [aux_sym_source_file_repeat1] = STATE(25),
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
    [aux_sym_source_file_repeat1] = STATE(25),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(27),
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
    [aux_sym_application_expression_repeat1] = STATE(20),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(20),
    [sym_binding] = STATE(29),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [12] = {
    [anon_sym_RBRACE] = ACTIONS(39),
    [anon_sym_RPAREN] = ACTIONS(39),
    [anon_sym_COMMA] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(39),
    [sym_identifier] = ACTIONS(39),
  },
  [13] = {
    [anon_sym_RBRACE] = ACTIONS(41),
    [anon_sym_RPAREN] = ACTIONS(41),
    [anon_sym_COMMA] = ACTIONS(41),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(41),
    [sym_identifier] = ACTIONS(41),
  },
  [14] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(43),
  },
  [15] = {
    [aux_sym_record_expression_repeat1] = STATE(36),
    [sym_record_expression_entry] = STATE(37),
    [sym_path] = STATE(38),
    [anon_sym_RBRACE] = ACTIONS(45),
    [anon_sym_TILDE] = ACTIONS(47),
    [anon_sym_DOT] = ACTIONS(49),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(51),
    [sym_identifier] = ACTIONS(53),
  },
  [16] = {
    [aux_sym_path_repeat1] = STATE(41),
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
    [aux_sym_application_expression_repeat1] = STATE(42),
    [sym_path] = STATE(42),
    [anon_sym_DOT] = ACTIONS(21),
    [anon_sym_EQ] = ACTIONS(65),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(65),
  },
  [21] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(67),
    [sym_identifier] = ACTIONS(67),
  },
  [22] = {
    [sym_parameter_list] = STATE(44),
    [aux_sym_parameter_list_repeat1] = STATE(45),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(69),
    [sym_identifier] = ACTIONS(71),
  },
  [23] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(73),
    [sym_identifier] = ACTIONS(73),
  },
  [24] = {
    [sym_type_definition_rhs] = STATE(49),
    [aux_sym_parameter_list_repeat1] = STATE(50),
    [sym_parameter_list] = STATE(51),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(75),
    [anon_sym_COLON] = ACTIONS(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(79),
    [sym_identifier] = ACTIONS(81),
  },
  [25] = {
    [sym_top_value_signature] = STATE(5),
    [sym_top_type_definition] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(25),
    [sym_top_value_definition] = STATE(5),
    [sym_top_type_signature] = STATE(5),
    [sym_definition] = STATE(55),
    [ts_builtin_sym_end] = ACTIONS(83),
    [anon_sym_type] = ACTIONS(85),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(88),
  },
  [26] = {
    [ts_builtin_sym_end] = ACTIONS(83),
    [anon_sym_type] = ACTIONS(83),
    [sym_comment] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(83),
  },
  [27] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(91),
  },
  [28] = {
    [aux_sym_path_repeat1] = STATE(41),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [29] = {
    [sym_parameter_list] = STATE(57),
    [aux_sym_parameter_list_repeat1] = STATE(45),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(93),
    [sym_identifier] = ACTIONS(71),
  },
  [30] = {
    [aux_sym_path_repeat1] = STATE(58),
    [anon_sym_DOT] = ACTIONS(95),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [31] = {
    [anon_sym_RBRACE] = ACTIONS(97),
    [anon_sym_RPAREN] = ACTIONS(97),
    [anon_sym_COMMA] = ACTIONS(97),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(97),
    [sym_identifier] = ACTIONS(97),
  },
  [32] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(99),
  },
  [33] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(101),
  },
  [34] = {
    [aux_sym_record_expression_repeat1] = STATE(63),
    [sym_record_expression_entry] = STATE(64),
    [anon_sym_RBRACE] = ACTIONS(103),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [35] = {
    [aux_sym_path_repeat1] = STATE(67),
    [anon_sym_DOT] = ACTIONS(107),
    [anon_sym_EQ] = ACTIONS(109),
    [anon_sym_with] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
  },
  [36] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(64),
    [anon_sym_RBRACE] = ACTIONS(103),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [37] = {
    [anon_sym_RBRACE] = ACTIONS(103),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [38] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(113),
  },
  [39] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(115),
  },
  [40] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(78),
    [sym_type] = STATE(79),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(119),
    [anon_sym_LPAREN] = ACTIONS(121),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(125),
  },
  [41] = {
    [aux_sym_path_repeat1] = STATE(80),
    [anon_sym_DOT] = ACTIONS(95),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [42] = {
    [aux_sym_application_expression_repeat1] = STATE(42),
    [sym_path] = STATE(42),
    [anon_sym_DOT] = ACTIONS(127),
    [anon_sym_EQ] = ACTIONS(130),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(132),
  },
  [43] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(89),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(89),
    [sym_expression] = STATE(90),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(141),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(145),
    [sym_identifier] = ACTIONS(147),
  },
  [44] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(93),
  },
  [45] = {
    [aux_sym_parameter_list_repeat1] = STATE(91),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(149),
    [sym_identifier] = ACTIONS(151),
  },
  [46] = {
    [sym__begin] = STATE(93),
    [sym_comment] = ACTIONS(3),
    [sym__indent] = ACTIONS(153),
    [anon_sym_LBRACE] = ACTIONS(155),
  },
  [47] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(97),
    [sym_type] = STATE(98),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(157),
    [anon_sym_LPAREN] = ACTIONS(159),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(161),
  },
  [48] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(78),
    [sym_type] = STATE(99),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(119),
    [anon_sym_LPAREN] = ACTIONS(121),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(125),
  },
  [49] = {
    [sym__align] = ACTIONS(163),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(163),
  },
  [50] = {
    [aux_sym_parameter_list_repeat1] = STATE(100),
    [anon_sym_where] = ACTIONS(165),
    [anon_sym_COLON] = ACTIONS(149),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(149),
    [sym_identifier] = ACTIONS(167),
  },
  [51] = {
    [sym_type_definition_rhs] = STATE(102),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(169),
    [anon_sym_COLON] = ACTIONS(171),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(79),
  },
  [52] = {
    [sym__align] = ACTIONS(173),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(173),
  },
  [53] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(20),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(20),
    [sym_binding] = STATE(105),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [anon_sym_rec] = ACTIONS(175),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(177),
  },
  [54] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(179),
  },
  [55] = {
    [sym__align] = ACTIONS(33),
    [sym_comment] = ACTIONS(3),
  },
  [56] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(89),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(89),
    [sym_expression] = STATE(107),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(141),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(145),
    [sym_identifier] = ACTIONS(147),
  },
  [57] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(181),
  },
  [58] = {
    [aux_sym_path_repeat1] = STATE(80),
    [anon_sym_DOT] = ACTIONS(183),
    [anon_sym_EQ] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [59] = {
    [anon_sym_RBRACE] = ACTIONS(185),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(185),
  },
  [60] = {
    [aux_sym_path_repeat1] = STATE(109),
    [anon_sym_DOT] = ACTIONS(107),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(95),
  },
  [61] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(109),
  },
  [62] = {
    [anon_sym_RBRACE] = ACTIONS(187),
    [anon_sym_RPAREN] = ACTIONS(187),
    [anon_sym_COMMA] = ACTIONS(187),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(187),
    [sym_identifier] = ACTIONS(187),
  },
  [63] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(111),
    [anon_sym_RBRACE] = ACTIONS(189),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [64] = {
    [anon_sym_RBRACE] = ACTIONS(189),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [65] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(191),
  },
  [66] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(119),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(119),
    [sym_expression] = STATE(120),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(195),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(199),
    [sym_identifier] = ACTIONS(201),
  },
  [67] = {
    [aux_sym_path_repeat1] = STATE(121),
    [anon_sym_DOT] = ACTIONS(107),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(95),
  },
  [68] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(123),
    [anon_sym_RBRACE] = ACTIONS(203),
    [anon_sym_TILDE] = ACTIONS(205),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(208),
  },
  [69] = {
    [anon_sym_RBRACE] = ACTIONS(203),
    [anon_sym_TILDE] = ACTIONS(203),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(203),
  },
  [70] = {
    [aux_sym_record_expression_repeat1] = STATE(124),
    [sym_record_expression_entry] = STATE(111),
    [anon_sym_RBRACE] = ACTIONS(189),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [71] = {
    [anon_sym_RPAREN] = ACTIONS(211),
    [anon_sym_COLON] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(211),
    [anon_sym_SEMI] = ACTIONS(211),
    [anon_sym_DASH_GT] = ACTIONS(211),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_COMMA] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [sym_identifier] = ACTIONS(211),
  },
  [72] = {
    [sym__align] = ACTIONS(213),
    [anon_sym_where] = ACTIONS(213),
    [ts_builtin_sym_end] = ACTIONS(213),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(213),
  },
  [73] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(215),
  },
  [74] = {
    [sym_path] = STATE(128),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [75] = {
    [sym_record_label] = STATE(131),
    [aux_sym_record_type_repeat1] = STATE(132),
    [anon_sym_RBRACE] = ACTIONS(221),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [76] = {
    [aux_sym_path_repeat1] = STATE(134),
    [sym__align] = ACTIONS(57),
    [ts_builtin_sym_end] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [77] = {
    [sym__align] = ACTIONS(227),
    [anon_sym_where] = ACTIONS(227),
    [ts_builtin_sym_end] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(227),
  },
  [78] = {
    [sym__align] = ACTIONS(227),
    [ts_builtin_sym_end] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(229),
  },
  [79] = {
    [sym__align] = ACTIONS(231),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(231),
  },
  [80] = {
    [aux_sym_path_repeat1] = STATE(80),
    [anon_sym_DOT] = ACTIONS(233),
    [anon_sym_EQ] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [81] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(141),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(142),
    [sym_record_expression_entry] = STATE(143),
    [sym_expression] = STATE(144),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [aux_sym_record_expression_repeat1] = STATE(145),
    [sym_identifier] = ACTIONS(236),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [anon_sym_RBRACE] = ACTIONS(238),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(240),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_with] = ACTIONS(242),
    [anon_sym_let] = ACTIONS(244),
  },
  [82] = {
    [sym__align] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(39),
  },
  [83] = {
    [sym__align] = ACTIONS(41),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(41),
  },
  [84] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(246),
  },
  [85] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(150),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(150),
    [sym_expression] = STATE(151),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(252),
  },
  [86] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(153),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [87] = {
    [aux_sym_path_repeat1] = STATE(154),
    [sym__align] = ACTIONS(57),
    [ts_builtin_sym_end] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [88] = {
    [sym__align] = ACTIONS(254),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(254),
  },
  [89] = {
    [aux_sym_application_expression_repeat1] = STATE(155),
    [sym_path] = STATE(155),
    [sym__align] = ACTIONS(65),
    [ts_builtin_sym_end] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(141),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(256),
  },
  [90] = {
    [sym__align] = ACTIONS(258),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(258),
  },
  [91] = {
    [aux_sym_parameter_list_repeat1] = STATE(91),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(260),
    [sym_identifier] = ACTIONS(262),
  },
  [92] = {
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(158),
    [sym_data_constructor_definition] = STATE(159),
    [anon_sym_RBRACE] = ACTIONS(265),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(267),
  },
  [93] = {
    [sym__end] = STATE(156),
    [sym_data_constructor_definition] = STATE(162),
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(161),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(265),
    [sym_identifier] = ACTIONS(269),
  },
  [94] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(271),
  },
  [95] = {
    [sym_path] = STATE(164),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [96] = {
    [aux_sym_path_repeat1] = STATE(166),
    [anon_sym_where] = ACTIONS(57),
    [sym__align] = ACTIONS(57),
    [ts_builtin_sym_end] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [97] = {
    [anon_sym_where] = ACTIONS(227),
    [sym__align] = ACTIONS(227),
    [ts_builtin_sym_end] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(227),
    [anon_sym_DASH_GT] = ACTIONS(275),
  },
  [98] = {
    [sym_type_definition_rhs] = STATE(168),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [sym__align] = ACTIONS(277),
    [anon_sym_where] = ACTIONS(169),
    [ts_builtin_sym_end] = ACTIONS(277),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(79),
  },
  [99] = {
    [sym__align] = ACTIONS(279),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(279),
  },
  [100] = {
    [aux_sym_parameter_list_repeat1] = STATE(100),
    [anon_sym_where] = ACTIONS(281),
    [anon_sym_COLON] = ACTIONS(260),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(260),
    [sym_identifier] = ACTIONS(283),
  },
  [101] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(175),
    [sym_type] = STATE(176),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(288),
    [anon_sym_LPAREN] = ACTIONS(290),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(294),
  },
  [102] = {
    [sym__align] = ACTIONS(296),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(296),
  },
  [103] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(20),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(20),
    [sym_binding] = STATE(177),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [104] = {
    [aux_sym_path_repeat1] = STATE(41),
    [anon_sym_COLON] = ACTIONS(298),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [105] = {
    [sym_parameter_list] = STATE(180),
    [aux_sym_parameter_list_repeat1] = STATE(45),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(300),
    [sym_identifier] = ACTIONS(71),
  },
  [106] = {
    [sym_type_definition_rhs] = STATE(49),
    [aux_sym_parameter_list_repeat1] = STATE(50),
    [sym_parameter_list] = STATE(183),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(75),
    [anon_sym_COLON] = ACTIONS(302),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(304),
    [sym_identifier] = ACTIONS(81),
  },
  [107] = {
    [sym__align] = ACTIONS(306),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(306),
  },
  [108] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(89),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(89),
    [sym_expression] = STATE(184),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(141),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(145),
    [sym_identifier] = ACTIONS(147),
  },
  [109] = {
    [aux_sym_path_repeat1] = STATE(121),
    [anon_sym_DOT] = ACTIONS(107),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(183),
  },
  [110] = {
    [anon_sym_RBRACE] = ACTIONS(308),
    [anon_sym_RPAREN] = ACTIONS(308),
    [anon_sym_COMMA] = ACTIONS(308),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(308),
    [sym_identifier] = ACTIONS(308),
  },
  [111] = {
    [anon_sym_RBRACE] = ACTIONS(310),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [112] = {
    [anon_sym_where] = ACTIONS(211),
    [anon_sym_DASH_GT] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(211),
    [anon_sym_EQ] = ACTIONS(211),
  },
  [113] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(141),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(186),
    [sym_record_expression_entry] = STATE(37),
    [sym_expression] = STATE(187),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [aux_sym_record_expression_repeat1] = STATE(36),
    [sym_identifier] = ACTIONS(236),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [anon_sym_RBRACE] = ACTIONS(45),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(240),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_with] = ACTIONS(51),
    [anon_sym_let] = ACTIONS(244),
  },
  [114] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(312),
  },
  [115] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(150),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(150),
    [sym_expression] = STATE(189),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(252),
  },
  [116] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(190),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [117] = {
    [aux_sym_path_repeat1] = STATE(191),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [118] = {
    [anon_sym_RBRACE] = ACTIONS(254),
    [anon_sym_RPAREN] = ACTIONS(254),
    [anon_sym_COMMA] = ACTIONS(254),
    [sym_comment] = ACTIONS(3),
    [anon_sym_in] = ACTIONS(254),
  },
  [119] = {
    [aux_sym_application_expression_repeat1] = STATE(192),
    [sym_path] = STATE(192),
    [anon_sym_RBRACE] = ACTIONS(65),
    [anon_sym_COMMA] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(195),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(314),
  },
  [120] = {
    [anon_sym_RBRACE] = ACTIONS(316),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(316),
  },
  [121] = {
    [aux_sym_path_repeat1] = STATE(121),
    [anon_sym_DOT] = ACTIONS(318),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(211),
  },
  [122] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(321),
  },
  [123] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [124] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(194),
    [anon_sym_RBRACE] = ACTIONS(310),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [125] = {
    [aux_sym_path_repeat1] = STATE(195),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [126] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(323),
  },
  [127] = {
    [aux_sym_path_repeat1] = STATE(197),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(57),
  },
  [128] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(327),
  },
  [129] = {
    [sym__align] = ACTIONS(329),
    [anon_sym_where] = ACTIONS(329),
    [ts_builtin_sym_end] = ACTIONS(329),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(329),
  },
  [130] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(331),
  },
  [131] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(333),
  },
  [132] = {
    [sym_record_label] = STATE(201),
    [aux_sym_record_type_repeat1] = STATE(202),
    [anon_sym_RBRACE] = ACTIONS(335),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [133] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(337),
  },
  [134] = {
    [aux_sym_path_repeat1] = STATE(204),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [135] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(78),
    [sym_type] = STATE(205),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(119),
    [anon_sym_LPAREN] = ACTIONS(121),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(125),
  },
  [136] = {
    [aux_sym_record_expression_repeat1] = STATE(207),
    [sym_record_expression_entry] = STATE(208),
    [anon_sym_RBRACE] = ACTIONS(339),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [137] = {
    [sym__align] = ACTIONS(97),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(97),
  },
  [138] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(341),
  },
  [139] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(210),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [140] = {
    [aux_sym_path_repeat1] = STATE(212),
    [anon_sym_RBRACE] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_EQ] = ACTIONS(109),
    [anon_sym_with] = ACTIONS(343),
    [sym_identifier] = ACTIONS(343),
  },
  [141] = {
    [aux_sym_application_expression_repeat1] = STATE(215),
    [sym_path] = STATE(215),
    [anon_sym_RBRACE] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(345),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(347),
  },
  [142] = {
    [anon_sym_RBRACE] = ACTIONS(349),
    [anon_sym_DOT] = ACTIONS(349),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(351),
    [sym_identifier] = ACTIONS(353),
  },
  [143] = {
    [anon_sym_RBRACE] = ACTIONS(339),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [144] = {
    [anon_sym_RBRACE] = ACTIONS(355),
    [sym_comment] = ACTIONS(3),
  },
  [145] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(208),
    [anon_sym_RBRACE] = ACTIONS(339),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [146] = {
    [aux_sym_path_repeat1] = STATE(218),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [147] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(357),
  },
  [148] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(220),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [149] = {
    [aux_sym_path_repeat1] = STATE(221),
    [anon_sym_RPAREN] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [150] = {
    [aux_sym_application_expression_repeat1] = STATE(222),
    [sym_path] = STATE(222),
    [anon_sym_RPAREN] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(248),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(359),
  },
  [151] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(361),
  },
  [152] = {
    [aux_sym_application_expression_repeat1] = STATE(42),
    [sym_path] = STATE(42),
    [anon_sym_DOT] = ACTIONS(21),
    [anon_sym_EQ] = ACTIONS(65),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(37),
  },
  [153] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(363),
  },
  [154] = {
    [aux_sym_path_repeat1] = STATE(225),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [155] = {
    [aux_sym_application_expression_repeat1] = STATE(155),
    [sym_path] = STATE(155),
    [sym__align] = ACTIONS(130),
    [ts_builtin_sym_end] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(365),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(368),
  },
  [156] = {
    [sym__align] = ACTIONS(371),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(371),
  },
  [157] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(373),
  },
  [158] = {
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(228),
    [sym_data_constructor_definition] = STATE(229),
    [anon_sym_RBRACE] = ACTIONS(375),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(267),
  },
  [159] = {
    [anon_sym_RBRACE] = ACTIONS(375),
    [anon_sym_SEMI] = ACTIONS(377),
    [sym_comment] = ACTIONS(3),
  },
  [160] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(379),
  },
  [161] = {
    [sym__end] = STATE(227),
    [sym_data_constructor_definition] = STATE(233),
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(232),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(375),
    [sym_identifier] = ACTIONS(269),
  },
  [162] = {
    [sym__end] = STATE(227),
    [sym__semi] = STATE(235),
    [sym__align] = ACTIONS(381),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(375),
  },
  [163] = {
    [aux_sym_path_repeat1] = STATE(236),
    [anon_sym_where] = ACTIONS(95),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [164] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(383),
  },
  [165] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(385),
  },
  [166] = {
    [aux_sym_path_repeat1] = STATE(239),
    [anon_sym_where] = ACTIONS(95),
    [sym__align] = ACTIONS(95),
    [ts_builtin_sym_end] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [167] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(97),
    [sym_type] = STATE(205),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(157),
    [anon_sym_LPAREN] = ACTIONS(159),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(161),
  },
  [168] = {
    [sym__align] = ACTIONS(387),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(387),
  },
  [169] = {
    [anon_sym_where] = ACTIONS(213),
    [anon_sym_RBRACE] = ACTIONS(213),
    [anon_sym_COMMA] = ACTIONS(213),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(213),
    [anon_sym_SEMI] = ACTIONS(213),
  },
  [170] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(389),
  },
  [171] = {
    [sym_path] = STATE(241),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [172] = {
    [sym_record_label] = STATE(243),
    [aux_sym_record_type_repeat1] = STATE(244),
    [anon_sym_RBRACE] = ACTIONS(391),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [173] = {
    [aux_sym_path_repeat1] = STATE(245),
    [anon_sym_where] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(107),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [174] = {
    [anon_sym_where] = ACTIONS(227),
    [anon_sym_RBRACE] = ACTIONS(227),
    [anon_sym_COMMA] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(227),
    [anon_sym_SEMI] = ACTIONS(227),
  },
  [175] = {
    [anon_sym_where] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(227),
    [anon_sym_DASH_GT] = ACTIONS(393),
  },
  [176] = {
    [sym_type_definition_rhs] = STATE(247),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(169),
    [anon_sym_EQ] = ACTIONS(79),
    [sym_comment] = ACTIONS(3),
  },
  [177] = {
    [sym_parameter_list] = STATE(249),
    [aux_sym_parameter_list_repeat1] = STATE(45),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(395),
    [sym_identifier] = ACTIONS(71),
  },
  [178] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(253),
    [sym_type] = STATE(79),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(397),
    [anon_sym_LPAREN] = ACTIONS(399),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(401),
  },
  [179] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(257),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(257),
    [sym_expression] = STATE(90),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(403),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(405),
    [sym_identifier] = ACTIONS(407),
  },
  [180] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(395),
  },
  [181] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(261),
    [sym_type] = STATE(262),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(409),
    [anon_sym_LPAREN] = ACTIONS(411),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(413),
  },
  [182] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(253),
    [sym_type] = STATE(99),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(397),
    [anon_sym_LPAREN] = ACTIONS(399),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(401),
  },
  [183] = {
    [sym_type_definition_rhs] = STATE(102),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(169),
    [anon_sym_COLON] = ACTIONS(415),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(304),
  },
  [184] = {
    [sym__align] = ACTIONS(417),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(417),
  },
  [185] = {
    [anon_sym_RBRACE] = ACTIONS(419),
    [anon_sym_RPAREN] = ACTIONS(419),
    [anon_sym_COMMA] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(419),
    [sym_identifier] = ACTIONS(419),
  },
  [186] = {
    [anon_sym_RBRACE] = ACTIONS(349),
    [anon_sym_DOT] = ACTIONS(349),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(421),
    [sym_identifier] = ACTIONS(353),
  },
  [187] = {
    [anon_sym_RBRACE] = ACTIONS(423),
    [sym_comment] = ACTIONS(3),
  },
  [188] = {
    [aux_sym_path_repeat1] = STATE(265),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [189] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(425),
  },
  [190] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(427),
  },
  [191] = {
    [aux_sym_path_repeat1] = STATE(268),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [192] = {
    [aux_sym_application_expression_repeat1] = STATE(192),
    [sym_path] = STATE(192),
    [anon_sym_RBRACE] = ACTIONS(130),
    [anon_sym_COMMA] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(429),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(432),
  },
  [193] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(272),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(272),
    [sym_expression] = STATE(120),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(435),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(437),
    [sym_identifier] = ACTIONS(439),
  },
  [194] = {
    [anon_sym_RBRACE] = ACTIONS(441),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [195] = {
    [aux_sym_path_repeat1] = STATE(204),
    [sym__align] = ACTIONS(183),
    [ts_builtin_sym_end] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [196] = {
    [aux_sym_path_repeat1] = STATE(274),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(95),
  },
  [197] = {
    [aux_sym_path_repeat1] = STATE(275),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(95),
  },
  [198] = {
    [sym_path] = STATE(278),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [199] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(283),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [200] = {
    [sym__align] = ACTIONS(453),
    [anon_sym_where] = ACTIONS(453),
    [ts_builtin_sym_end] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(453),
  },
  [201] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(455),
  },
  [202] = {
    [sym_record_label] = STATE(285),
    [aux_sym_record_type_repeat1] = STATE(202),
    [anon_sym_RBRACE] = ACTIONS(457),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(459),
  },
  [203] = {
    [sym__align] = ACTIONS(211),
    [sym_identifier] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [204] = {
    [aux_sym_path_repeat1] = STATE(204),
    [sym__align] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(462),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [205] = {
    [sym__align] = ACTIONS(465),
    [anon_sym_where] = ACTIONS(465),
    [ts_builtin_sym_end] = ACTIONS(465),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(465),
  },
  [206] = {
    [sym__align] = ACTIONS(187),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(187),
  },
  [207] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(287),
    [anon_sym_RBRACE] = ACTIONS(467),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [208] = {
    [anon_sym_RBRACE] = ACTIONS(467),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [209] = {
    [aux_sym_path_repeat1] = STATE(288),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(469),
    [sym_identifier] = ACTIONS(469),
  },
  [210] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(471),
  },
  [211] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(473),
  },
  [212] = {
    [aux_sym_path_repeat1] = STATE(291),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(469),
    [sym_identifier] = ACTIONS(469),
  },
  [213] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(475),
  },
  [214] = {
    [aux_sym_path_repeat1] = STATE(293),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [215] = {
    [aux_sym_application_expression_repeat1] = STATE(215),
    [sym_path] = STATE(215),
    [anon_sym_RBRACE] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(477),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(480),
  },
  [216] = {
    [aux_sym_record_expression_repeat1] = STATE(294),
    [sym_record_expression_entry] = STATE(287),
    [anon_sym_RBRACE] = ACTIONS(467),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [217] = {
    [sym__align] = ACTIONS(483),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(483),
  },
  [218] = {
    [aux_sym_path_repeat1] = STATE(225),
    [sym__align] = ACTIONS(183),
    [ts_builtin_sym_end] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [219] = {
    [aux_sym_path_repeat1] = STATE(295),
    [anon_sym_RPAREN] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [220] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(485),
  },
  [221] = {
    [aux_sym_path_repeat1] = STATE(297),
    [anon_sym_RPAREN] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [222] = {
    [aux_sym_application_expression_repeat1] = STATE(222),
    [sym_path] = STATE(222),
    [anon_sym_RPAREN] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(487),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(490),
  },
  [223] = {
    [sym__align] = ACTIONS(493),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(493),
  },
  [224] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(305),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [225] = {
    [aux_sym_path_repeat1] = STATE(225),
    [sym__align] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(462),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [226] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(309),
    [sym_type] = STATE(310),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(507),
    [anon_sym_LPAREN] = ACTIONS(509),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(511),
  },
  [227] = {
    [sym__align] = ACTIONS(513),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(513),
  },
  [228] = {
    [aux_sym_data_type_definition_rhs_repeat1] = STATE(228),
    [sym_data_constructor_definition] = STATE(312),
    [anon_sym_RBRACE] = ACTIONS(515),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(517),
  },
  [229] = {
    [anon_sym_RBRACE] = ACTIONS(520),
    [anon_sym_SEMI] = ACTIONS(377),
    [sym_comment] = ACTIONS(3),
  },
  [230] = {
    [anon_sym_RBRACE] = ACTIONS(515),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(515),
  },
  [231] = {
    [sym_record_type] = STATE(319),
    [sym_path] = STATE(320),
    [sym_type] = STATE(321),
    [sym_function_type] = STATE(319),
    [sym_unit_type] = STATE(319),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(522),
    [anon_sym_DOT] = ACTIONS(524),
    [anon_sym_LPAREN] = ACTIONS(526),
    [anon_sym_LBRACE] = ACTIONS(528),
    [sym_identifier] = ACTIONS(530),
  },
  [232] = {
    [sym_data_constructor_definition] = STATE(323),
    [aux_sym_data_type_definition_rhs_repeat2] = STATE(232),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(532),
    [sym_identifier] = ACTIONS(534),
  },
  [233] = {
    [sym__end] = STATE(313),
    [sym__semi] = STATE(235),
    [sym__align] = ACTIONS(381),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(520),
  },
  [234] = {
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(537),
    [sym_identifier] = ACTIONS(537),
  },
  [235] = {
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(532),
    [sym_identifier] = ACTIONS(532),
  },
  [236] = {
    [aux_sym_path_repeat1] = STATE(239),
    [anon_sym_where] = ACTIONS(183),
    [sym__align] = ACTIONS(183),
    [ts_builtin_sym_end] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [237] = {
    [sym_path] = STATE(324),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [238] = {
    [anon_sym_where] = ACTIONS(211),
    [sym__align] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [anon_sym_EQ] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [239] = {
    [aux_sym_path_repeat1] = STATE(239),
    [anon_sym_where] = ACTIONS(211),
    [sym__align] = ACTIONS(211),
    [ts_builtin_sym_end] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(539),
    [anon_sym_EQ] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [240] = {
    [aux_sym_path_repeat1] = STATE(325),
    [anon_sym_where] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(107),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [241] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(542),
  },
  [242] = {
    [anon_sym_where] = ACTIONS(329),
    [anon_sym_RBRACE] = ACTIONS(329),
    [anon_sym_COMMA] = ACTIONS(329),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(329),
    [anon_sym_SEMI] = ACTIONS(329),
  },
  [243] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(544),
  },
  [244] = {
    [sym_record_label] = STATE(329),
    [aux_sym_record_type_repeat1] = STATE(202),
    [anon_sym_RBRACE] = ACTIONS(546),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [245] = {
    [aux_sym_path_repeat1] = STATE(330),
    [anon_sym_where] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(107),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [246] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(175),
    [sym_type] = STATE(331),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(288),
    [anon_sym_LPAREN] = ACTIONS(290),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(294),
  },
  [247] = {
    [sym__align] = ACTIONS(548),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(548),
  },
  [248] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(257),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(257),
    [sym_expression] = STATE(107),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(403),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(405),
    [sym_identifier] = ACTIONS(407),
  },
  [249] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(550),
  },
  [250] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(552),
  },
  [251] = {
    [sym_path] = STATE(334),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [252] = {
    [aux_sym_path_repeat1] = STATE(335),
    [sym__align] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [253] = {
    [sym__align] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(554),
  },
  [254] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(556),
  },
  [255] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(338),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [256] = {
    [aux_sym_path_repeat1] = STATE(339),
    [sym__align] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [257] = {
    [aux_sym_application_expression_repeat1] = STATE(340),
    [sym_path] = STATE(340),
    [sym__align] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(403),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(558),
  },
  [258] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(560),
  },
  [259] = {
    [sym_path] = STATE(342),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [260] = {
    [aux_sym_path_repeat1] = STATE(343),
    [anon_sym_where] = ACTIONS(57),
    [sym__align] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [261] = {
    [anon_sym_where] = ACTIONS(227),
    [sym__align] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(227),
    [anon_sym_DASH_GT] = ACTIONS(562),
  },
  [262] = {
    [sym_type_definition_rhs] = STATE(168),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [sym__align] = ACTIONS(277),
    [anon_sym_where] = ACTIONS(169),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(304),
  },
  [263] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(175),
    [sym_type] = STATE(345),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(288),
    [anon_sym_LPAREN] = ACTIONS(290),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(294),
  },
  [264] = {
    [anon_sym_RBRACE] = ACTIONS(483),
    [anon_sym_RPAREN] = ACTIONS(483),
    [anon_sym_COMMA] = ACTIONS(483),
    [sym_comment] = ACTIONS(3),
    [anon_sym_in] = ACTIONS(483),
  },
  [265] = {
    [aux_sym_path_repeat1] = STATE(268),
    [anon_sym_RBRACE] = ACTIONS(183),
    [anon_sym_COMMA] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [266] = {
    [anon_sym_RBRACE] = ACTIONS(493),
    [anon_sym_RPAREN] = ACTIONS(493),
    [anon_sym_COMMA] = ACTIONS(493),
    [sym_comment] = ACTIONS(3),
    [anon_sym_in] = ACTIONS(493),
  },
  [267] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(346),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [268] = {
    [aux_sym_path_repeat1] = STATE(268),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_COMMA] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [269] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(564),
  },
  [270] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(348),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [271] = {
    [aux_sym_path_repeat1] = STATE(349),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(57),
  },
  [272] = {
    [aux_sym_application_expression_repeat1] = STATE(350),
    [sym_path] = STATE(350),
    [anon_sym_COMMA] = ACTIONS(65),
    [anon_sym_DOT] = ACTIONS(435),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(566),
  },
  [273] = {
    [anon_sym_RBRACE] = ACTIONS(568),
    [anon_sym_RPAREN] = ACTIONS(568),
    [anon_sym_COMMA] = ACTIONS(568),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(568),
    [sym_identifier] = ACTIONS(568),
  },
  [274] = {
    [aux_sym_path_repeat1] = STATE(275),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(183),
  },
  [275] = {
    [aux_sym_path_repeat1] = STATE(275),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(211),
  },
  [276] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(570),
  },
  [277] = {
    [aux_sym_path_repeat1] = STATE(352),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(57),
  },
  [278] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(572),
  },
  [279] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(574),
  },
  [280] = {
    [sym_path] = STATE(355),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [281] = {
    [aux_sym_path_repeat1] = STATE(356),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [282] = {
    [anon_sym_RBRACE] = ACTIONS(227),
    [anon_sym_COMMA] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(576),
  },
  [283] = {
    [anon_sym_RBRACE] = ACTIONS(578),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [284] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(360),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [285] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(582),
  },
  [286] = {
    [sym__align] = ACTIONS(308),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(308),
  },
  [287] = {
    [anon_sym_RBRACE] = ACTIONS(584),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [288] = {
    [aux_sym_path_repeat1] = STATE(291),
    [anon_sym_RBRACE] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(586),
    [sym_identifier] = ACTIONS(586),
  },
  [289] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(363),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [290] = {
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(588),
    [sym_identifier] = ACTIONS(588),
  },
  [291] = {
    [aux_sym_path_repeat1] = STATE(291),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(590),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(588),
    [sym_identifier] = ACTIONS(588),
  },
  [292] = {
    [aux_sym_path_repeat1] = STATE(364),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [293] = {
    [aux_sym_path_repeat1] = STATE(365),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [294] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(366),
    [anon_sym_RBRACE] = ACTIONS(584),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [295] = {
    [aux_sym_path_repeat1] = STATE(297),
    [anon_sym_RPAREN] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [296] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(367),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [297] = {
    [aux_sym_path_repeat1] = STATE(297),
    [anon_sym_RPAREN] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [298] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(141),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(370),
    [sym_record_expression_entry] = STATE(371),
    [sym_expression] = STATE(187),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [aux_sym_record_expression_repeat1] = STATE(372),
    [sym_identifier] = ACTIONS(236),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [anon_sym_RBRACE] = ACTIONS(593),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(240),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_with] = ACTIONS(595),
    [anon_sym_let] = ACTIONS(244),
  },
  [299] = {
    [anon_sym_in] = ACTIONS(39),
    [sym_comment] = ACTIONS(3),
  },
  [300] = {
    [anon_sym_in] = ACTIONS(41),
    [sym_comment] = ACTIONS(3),
  },
  [301] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(597),
  },
  [302] = {
    [sym_pattern] = STATE(17),
    [sym_application_pattern] = STATE(18),
    [sym_atomic_expression] = STATE(19),
    [sym_atomic_pattern] = STATE(18),
    [aux_sym_application_expression_repeat1] = STATE(152),
    [sym_record_expression] = STATE(21),
    [sym_path] = STATE(152),
    [sym_binding] = STATE(374),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(23),
    [sym_record_pattern] = STATE(18),
    [sym_integer] = ACTIONS(17),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [anon_sym_DOT] = ACTIONS(21),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(23),
    [sym_identifier] = ACTIONS(37),
  },
  [303] = {
    [aux_sym_path_repeat1] = STATE(376),
    [anon_sym_DOT] = ACTIONS(57),
    [anon_sym_in] = ACTIONS(343),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(343),
  },
  [304] = {
    [aux_sym_application_expression_repeat1] = STATE(377),
    [sym_path] = STATE(377),
    [anon_sym_in] = ACTIONS(599),
    [anon_sym_DOT] = ACTIONS(501),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(505),
  },
  [305] = {
    [anon_sym_in] = ACTIONS(601),
    [sym_comment] = ACTIONS(3),
  },
  [306] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(603),
  },
  [307] = {
    [sym_path] = STATE(380),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [308] = {
    [aux_sym_path_repeat1] = STATE(381),
    [anon_sym_RBRACE] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [309] = {
    [anon_sym_RBRACE] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(227),
    [anon_sym_DASH_GT] = ACTIONS(605),
  },
  [310] = {
    [anon_sym_RBRACE] = ACTIONS(607),
    [anon_sym_SEMI] = ACTIONS(607),
    [sym_comment] = ACTIONS(3),
  },
  [311] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(609),
  },
  [312] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(377),
  },
  [313] = {
    [sym__align] = ACTIONS(611),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(611),
  },
  [314] = {
    [sym__align] = ACTIONS(213),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(213),
  },
  [315] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(613),
  },
  [316] = {
    [sym_path] = STATE(385),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [317] = {
    [sym_record_label] = STATE(387),
    [aux_sym_record_type_repeat1] = STATE(388),
    [anon_sym_RBRACE] = ACTIONS(615),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [318] = {
    [aux_sym_path_repeat1] = STATE(390),
    [sym__align] = ACTIONS(57),
    [sym__dedent] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(617),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [319] = {
    [sym__align] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(227),
  },
  [320] = {
    [sym__align] = ACTIONS(227),
    [sym__dedent] = ACTIONS(227),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(619),
  },
  [321] = {
    [sym__align] = ACTIONS(607),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(607),
  },
  [322] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(621),
  },
  [323] = {
    [sym__semi] = STATE(235),
    [sym__align] = ACTIONS(381),
    [sym_comment] = ACTIONS(3),
  },
  [324] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(623),
  },
  [325] = {
    [aux_sym_path_repeat1] = STATE(330),
    [anon_sym_where] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(107),
    [anon_sym_EQ] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [326] = {
    [sym_path] = STATE(394),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [327] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(395),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [328] = {
    [anon_sym_where] = ACTIONS(453),
    [anon_sym_RBRACE] = ACTIONS(453),
    [anon_sym_COMMA] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(453),
    [anon_sym_SEMI] = ACTIONS(453),
  },
  [329] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(625),
  },
  [330] = {
    [aux_sym_path_repeat1] = STATE(330),
    [anon_sym_where] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(318),
    [anon_sym_EQ] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [331] = {
    [anon_sym_where] = ACTIONS(465),
    [anon_sym_RBRACE] = ACTIONS(465),
    [anon_sym_COMMA] = ACTIONS(465),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(465),
    [anon_sym_SEMI] = ACTIONS(465),
  },
  [332] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(257),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(257),
    [sym_expression] = STATE(184),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(403),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(405),
    [sym_identifier] = ACTIONS(407),
  },
  [333] = {
    [aux_sym_path_repeat1] = STATE(397),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [334] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(627),
  },
  [335] = {
    [aux_sym_path_repeat1] = STATE(399),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [336] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(253),
    [sym_type] = STATE(205),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(397),
    [anon_sym_LPAREN] = ACTIONS(399),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(401),
  },
  [337] = {
    [aux_sym_path_repeat1] = STATE(400),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [338] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(629),
  },
  [339] = {
    [aux_sym_path_repeat1] = STATE(402),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [340] = {
    [aux_sym_application_expression_repeat1] = STATE(340),
    [sym_path] = STATE(340),
    [sym__align] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(631),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(634),
  },
  [341] = {
    [aux_sym_path_repeat1] = STATE(403),
    [anon_sym_where] = ACTIONS(95),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [342] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(637),
  },
  [343] = {
    [aux_sym_path_repeat1] = STATE(405),
    [anon_sym_where] = ACTIONS(95),
    [sym__align] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [344] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(261),
    [sym_type] = STATE(205),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(409),
    [anon_sym_LPAREN] = ACTIONS(411),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(413),
  },
  [345] = {
    [sym_type_definition_rhs] = STATE(247),
    [sym_data_type_definition_rhs] = STATE(52),
    [sym_type_alias_definition_rhs] = STATE(52),
    [anon_sym_where] = ACTIONS(169),
    [anon_sym_EQ] = ACTIONS(304),
    [sym_comment] = ACTIONS(3),
  },
  [346] = {
    [anon_sym_in] = ACTIONS(639),
    [sym_comment] = ACTIONS(3),
  },
  [347] = {
    [aux_sym_path_repeat1] = STATE(407),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [348] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(641),
  },
  [349] = {
    [aux_sym_path_repeat1] = STATE(409),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(95),
  },
  [350] = {
    [aux_sym_application_expression_repeat1] = STATE(350),
    [sym_path] = STATE(350),
    [anon_sym_COMMA] = ACTIONS(130),
    [anon_sym_DOT] = ACTIONS(643),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(646),
  },
  [351] = {
    [aux_sym_path_repeat1] = STATE(410),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(95),
  },
  [352] = {
    [aux_sym_path_repeat1] = STATE(411),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(95),
  },
  [353] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(649),
  },
  [354] = {
    [aux_sym_path_repeat1] = STATE(413),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [355] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(651),
  },
  [356] = {
    [aux_sym_path_repeat1] = STATE(415),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [357] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(331),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [358] = {
    [sym__align] = ACTIONS(653),
    [anon_sym_where] = ACTIONS(653),
    [ts_builtin_sym_end] = ACTIONS(653),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(653),
  },
  [359] = {
    [anon_sym_RBRACE] = ACTIONS(655),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(655),
  },
  [360] = {
    [anon_sym_RBRACE] = ACTIONS(657),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [361] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(420),
    [sym_type] = STATE(421),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(659),
    [anon_sym_LPAREN] = ACTIONS(661),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(663),
  },
  [362] = {
    [sym__align] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(419),
  },
  [363] = {
    [anon_sym_in] = ACTIONS(665),
    [sym_comment] = ACTIONS(3),
  },
  [364] = {
    [aux_sym_path_repeat1] = STATE(365),
    [anon_sym_RBRACE] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [365] = {
    [aux_sym_path_repeat1] = STATE(365),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [366] = {
    [anon_sym_RBRACE] = ACTIONS(667),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [367] = {
    [anon_sym_in] = ACTIONS(669),
    [sym_comment] = ACTIONS(3),
  },
  [368] = {
    [aux_sym_record_expression_repeat1] = STATE(426),
    [sym_record_expression_entry] = STATE(427),
    [anon_sym_RBRACE] = ACTIONS(671),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [369] = {
    [anon_sym_in] = ACTIONS(97),
    [sym_comment] = ACTIONS(3),
  },
  [370] = {
    [anon_sym_RBRACE] = ACTIONS(349),
    [anon_sym_DOT] = ACTIONS(349),
    [sym_comment] = ACTIONS(3),
    [anon_sym_with] = ACTIONS(673),
    [sym_identifier] = ACTIONS(353),
  },
  [371] = {
    [anon_sym_RBRACE] = ACTIONS(671),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [372] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(427),
    [anon_sym_RBRACE] = ACTIONS(671),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [373] = {
    [aux_sym_path_repeat1] = STATE(429),
    [anon_sym_DOT] = ACTIONS(95),
    [anon_sym_in] = ACTIONS(469),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(469),
  },
  [374] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(675),
  },
  [375] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(677),
  },
  [376] = {
    [aux_sym_path_repeat1] = STATE(432),
    [anon_sym_DOT] = ACTIONS(95),
    [anon_sym_in] = ACTIONS(469),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(469),
  },
  [377] = {
    [aux_sym_application_expression_repeat1] = STATE(377),
    [sym_path] = STATE(377),
    [anon_sym_DOT] = ACTIONS(679),
    [anon_sym_in] = ACTIONS(682),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(684),
  },
  [378] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(89),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(89),
    [sym_expression] = STATE(433),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(141),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(145),
    [sym_identifier] = ACTIONS(147),
  },
  [379] = {
    [aux_sym_path_repeat1] = STATE(434),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [380] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(687),
  },
  [381] = {
    [aux_sym_path_repeat1] = STATE(436),
    [anon_sym_RBRACE] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [382] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(309),
    [sym_type] = STATE(331),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(507),
    [anon_sym_LPAREN] = ACTIONS(509),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(511),
  },
  [383] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(440),
    [sym_type] = STATE(310),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(689),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(693),
  },
  [384] = {
    [aux_sym_path_repeat1] = STATE(441),
    [sym__align] = ACTIONS(95),
    [sym__dedent] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(617),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [385] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(695),
  },
  [386] = {
    [sym__align] = ACTIONS(329),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(329),
  },
  [387] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(697),
  },
  [388] = {
    [sym_record_label] = STATE(445),
    [aux_sym_record_type_repeat1] = STATE(202),
    [anon_sym_RBRACE] = ACTIONS(699),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(223),
  },
  [389] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(701),
  },
  [390] = {
    [aux_sym_path_repeat1] = STATE(447),
    [sym__align] = ACTIONS(95),
    [sym__dedent] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(617),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [391] = {
    [sym_record_type] = STATE(319),
    [sym_path] = STATE(320),
    [sym_type] = STATE(448),
    [sym_function_type] = STATE(319),
    [sym_unit_type] = STATE(319),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(522),
    [anon_sym_DOT] = ACTIONS(524),
    [anon_sym_LPAREN] = ACTIONS(526),
    [anon_sym_LBRACE] = ACTIONS(528),
    [sym_identifier] = ACTIONS(530),
  },
  [392] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(253),
    [sym_type] = STATE(449),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(397),
    [anon_sym_LPAREN] = ACTIONS(399),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(401),
  },
  [393] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(703),
  },
  [394] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(705),
  },
  [395] = {
    [anon_sym_RBRACE] = ACTIONS(707),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [396] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(453),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [397] = {
    [aux_sym_path_repeat1] = STATE(399),
    [sym__align] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(225),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [398] = {
    [sym_path] = STATE(454),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [399] = {
    [aux_sym_path_repeat1] = STATE(399),
    [sym__align] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(462),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [400] = {
    [aux_sym_path_repeat1] = STATE(402),
    [sym__align] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [401] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(455),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [402] = {
    [aux_sym_path_repeat1] = STATE(402),
    [sym__align] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(462),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [403] = {
    [aux_sym_path_repeat1] = STATE(405),
    [anon_sym_where] = ACTIONS(183),
    [sym__align] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(273),
    [anon_sym_EQ] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [404] = {
    [sym_path] = STATE(456),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [405] = {
    [aux_sym_path_repeat1] = STATE(405),
    [anon_sym_where] = ACTIONS(211),
    [sym__align] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(539),
    [anon_sym_EQ] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [406] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(119),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(119),
    [sym_expression] = STATE(457),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(195),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(199),
    [sym_identifier] = ACTIONS(201),
  },
  [407] = {
    [aux_sym_path_repeat1] = STATE(409),
    [anon_sym_COMMA] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(183),
  },
  [408] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(458),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [409] = {
    [aux_sym_path_repeat1] = STATE(409),
    [anon_sym_COMMA] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(211),
  },
  [410] = {
    [aux_sym_path_repeat1] = STATE(411),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(183),
  },
  [411] = {
    [aux_sym_path_repeat1] = STATE(411),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(211),
  },
  [412] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(78),
    [sym_type] = STATE(459),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(119),
    [anon_sym_LPAREN] = ACTIONS(121),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(125),
  },
  [413] = {
    [aux_sym_path_repeat1] = STATE(415),
    [anon_sym_RBRACE] = ACTIONS(183),
    [anon_sym_COMMA] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [414] = {
    [sym_path] = STATE(460),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [415] = {
    [aux_sym_path_repeat1] = STATE(415),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_COMMA] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [416] = {
    [sym__align] = ACTIONS(709),
    [anon_sym_where] = ACTIONS(709),
    [ts_builtin_sym_end] = ACTIONS(709),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(709),
  },
  [417] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(711),
  },
  [418] = {
    [sym_path] = STATE(462),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [419] = {
    [aux_sym_path_repeat1] = STATE(463),
    [anon_sym_COMMA] = ACTIONS(57),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [420] = {
    [anon_sym_DASH_GT] = ACTIONS(713),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(227),
  },
  [421] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [422] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(141),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(141),
    [sym_expression] = STATE(457),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(345),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(244),
    [sym_identifier] = ACTIONS(715),
  },
  [423] = {
    [sym__align] = ACTIONS(568),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(568),
  },
  [424] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(150),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(150),
    [sym_expression] = STATE(457),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(248),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(250),
    [sym_identifier] = ACTIONS(252),
  },
  [425] = {
    [anon_sym_in] = ACTIONS(187),
    [sym_comment] = ACTIONS(3),
  },
  [426] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(466),
    [anon_sym_RBRACE] = ACTIONS(717),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [427] = {
    [anon_sym_RBRACE] = ACTIONS(717),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [428] = {
    [aux_sym_record_expression_repeat1] = STATE(467),
    [sym_record_expression_entry] = STATE(466),
    [anon_sym_RBRACE] = ACTIONS(717),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [429] = {
    [aux_sym_path_repeat1] = STATE(432),
    [anon_sym_DOT] = ACTIONS(183),
    [anon_sym_in] = ACTIONS(586),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(586),
  },
  [430] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(468),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [431] = {
    [anon_sym_DOT] = ACTIONS(211),
    [anon_sym_in] = ACTIONS(588),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(588),
  },
  [432] = {
    [aux_sym_path_repeat1] = STATE(432),
    [anon_sym_DOT] = ACTIONS(719),
    [anon_sym_in] = ACTIONS(588),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(588),
  },
  [433] = {
    [sym__align] = ACTIONS(722),
    [sym_comment] = ACTIONS(3),
    [ts_builtin_sym_end] = ACTIONS(722),
  },
  [434] = {
    [aux_sym_path_repeat1] = STATE(436),
    [anon_sym_RBRACE] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [435] = {
    [sym_path] = STATE(469),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [436] = {
    [aux_sym_path_repeat1] = STATE(436),
    [anon_sym_RBRACE] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [anon_sym_SEMI] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [437] = {
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(724),
  },
  [438] = {
    [sym_path] = STATE(471),
    [anon_sym_DOT] = ACTIONS(217),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(219),
  },
  [439] = {
    [aux_sym_path_repeat1] = STATE(472),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(57),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(57),
  },
  [440] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_SEMI] = ACTIONS(227),
    [anon_sym_DASH_GT] = ACTIONS(726),
  },
  [441] = {
    [aux_sym_path_repeat1] = STATE(447),
    [sym__align] = ACTIONS(183),
    [sym__dedent] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(617),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [442] = {
    [sym_path] = STATE(474),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [443] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(475),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [444] = {
    [sym__align] = ACTIONS(453),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(453),
  },
  [445] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(728),
  },
  [446] = {
    [sym__align] = ACTIONS(211),
    [sym__dedent] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [447] = {
    [aux_sym_path_repeat1] = STATE(447),
    [sym__align] = ACTIONS(211),
    [sym__dedent] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(730),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [448] = {
    [sym__align] = ACTIONS(465),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(465),
  },
  [449] = {
    [sym__align] = ACTIONS(607),
    [sym_comment] = ACTIONS(3),
  },
  [450] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(97),
    [sym_type] = STATE(459),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(157),
    [anon_sym_LPAREN] = ACTIONS(159),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(161),
  },
  [451] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(733),
  },
  [452] = {
    [anon_sym_where] = ACTIONS(653),
    [anon_sym_RBRACE] = ACTIONS(653),
    [anon_sym_COMMA] = ACTIONS(653),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(653),
    [anon_sym_SEMI] = ACTIONS(653),
  },
  [453] = {
    [anon_sym_RBRACE] = ACTIONS(735),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [454] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(737),
  },
  [455] = {
    [anon_sym_in] = ACTIONS(739),
    [sym_comment] = ACTIONS(3),
  },
  [456] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(741),
  },
  [457] = {
    [anon_sym_RBRACE] = ACTIONS(722),
    [anon_sym_RPAREN] = ACTIONS(722),
    [anon_sym_COMMA] = ACTIONS(722),
    [sym_comment] = ACTIONS(3),
    [anon_sym_in] = ACTIONS(722),
  },
  [458] = {
    [anon_sym_in] = ACTIONS(743),
    [sym_comment] = ACTIONS(3),
  },
  [459] = {
    [sym__align] = ACTIONS(745),
    [anon_sym_where] = ACTIONS(745),
    [ts_builtin_sym_end] = ACTIONS(745),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(745),
  },
  [460] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(747),
  },
  [461] = {
    [aux_sym_path_repeat1] = STATE(484),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [462] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(749),
  },
  [463] = {
    [aux_sym_path_repeat1] = STATE(486),
    [anon_sym_COMMA] = ACTIONS(95),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [464] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(420),
    [sym_type] = STATE(331),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(659),
    [anon_sym_LPAREN] = ACTIONS(661),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(663),
  },
  [465] = {
    [anon_sym_in] = ACTIONS(308),
    [sym_comment] = ACTIONS(3),
  },
  [466] = {
    [anon_sym_RBRACE] = ACTIONS(751),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [467] = {
    [aux_sym_record_expression_repeat1] = STATE(68),
    [sym_record_expression_entry] = STATE(488),
    [anon_sym_RBRACE] = ACTIONS(751),
    [anon_sym_TILDE] = ACTIONS(47),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(105),
  },
  [468] = {
    [anon_sym_in] = ACTIONS(753),
    [sym_comment] = ACTIONS(3),
  },
  [469] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(755),
  },
  [470] = {
    [aux_sym_path_repeat1] = STATE(491),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [471] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_COLON] = ACTIONS(757),
  },
  [472] = {
    [aux_sym_path_repeat1] = STATE(493),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(95),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(95),
  },
  [473] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(440),
    [sym_type] = STATE(331),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(689),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(693),
  },
  [474] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(759),
  },
  [475] = {
    [anon_sym_RBRACE] = ACTIONS(761),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [476] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(496),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [477] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(175),
    [sym_type] = STATE(497),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(288),
    [anon_sym_LPAREN] = ACTIONS(290),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(294),
  },
  [478] = {
    [anon_sym_where] = ACTIONS(709),
    [anon_sym_RBRACE] = ACTIONS(709),
    [anon_sym_COMMA] = ACTIONS(709),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(709),
    [anon_sym_SEMI] = ACTIONS(709),
  },
  [479] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(763),
  },
  [480] = {
    [sym_let_binding] = STATE(88),
    [sym_suspension] = STATE(88),
    [sym_atomic_expression] = STATE(88),
    [aux_sym_application_expression_repeat1] = STATE(257),
    [sym_record_expression] = STATE(88),
    [sym_path] = STATE(257),
    [sym_expression] = STATE(433),
    [sym_unit] = STATE(82),
    [sym_application_expression] = STATE(88),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(135),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(137),
    [sym_integer] = ACTIONS(139),
    [anon_sym_DOT] = ACTIONS(403),
    [anon_sym_LPAREN] = ACTIONS(143),
    [anon_sym_let] = ACTIONS(405),
    [sym_identifier] = ACTIONS(407),
  },
  [481] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(765),
  },
  [482] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(272),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(272),
    [sym_expression] = STATE(457),
    [sym_unit] = STATE(12),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(19),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(193),
    [sym_integer] = ACTIONS(17),
    [anon_sym_DOT] = ACTIONS(435),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(437),
    [sym_identifier] = ACTIONS(439),
  },
  [483] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(767),
  },
  [484] = {
    [aux_sym_path_repeat1] = STATE(486),
    [anon_sym_COMMA] = ACTIONS(183),
    [anon_sym_DOT] = ACTIONS(325),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [485] = {
    [sym_path] = STATE(501),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [486] = {
    [aux_sym_path_repeat1] = STATE(486),
    [anon_sym_COMMA] = ACTIONS(211),
    [anon_sym_DOT] = ACTIONS(233),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [487] = {
    [anon_sym_in] = ACTIONS(419),
    [sym_comment] = ACTIONS(3),
  },
  [488] = {
    [anon_sym_RBRACE] = ACTIONS(769),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(111),
  },
  [489] = {
    [sym_let_binding] = STATE(118),
    [sym_suspension] = STATE(118),
    [sym_atomic_expression] = STATE(118),
    [aux_sym_application_expression_repeat1] = STATE(304),
    [sym_record_expression] = STATE(118),
    [sym_path] = STATE(304),
    [sym_expression] = STATE(457),
    [sym_unit] = STATE(299),
    [sym_application_expression] = STATE(118),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(495),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LBRACE] = ACTIONS(497),
    [sym_integer] = ACTIONS(499),
    [anon_sym_DOT] = ACTIONS(501),
    [anon_sym_LPAREN] = ACTIONS(197),
    [anon_sym_let] = ACTIONS(503),
    [sym_identifier] = ACTIONS(505),
  },
  [490] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(771),
  },
  [491] = {
    [aux_sym_path_repeat1] = STATE(493),
    [anon_sym_DOT] = ACTIONS(325),
    [anon_sym_SEMI] = ACTIONS(183),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(183),
  },
  [492] = {
    [sym_path] = STATE(504),
    [anon_sym_DOT] = ACTIONS(443),
    [sym_comment] = ACTIONS(3),
    [sym_identifier] = ACTIONS(445),
  },
  [493] = {
    [aux_sym_path_repeat1] = STATE(493),
    [anon_sym_DOT] = ACTIONS(233),
    [anon_sym_SEMI] = ACTIONS(211),
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(211),
  },
  [494] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(773),
  },
  [495] = {
    [sym__align] = ACTIONS(653),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(653),
  },
  [496] = {
    [anon_sym_RBRACE] = ACTIONS(775),
    [sym_comment] = ACTIONS(3),
    [anon_sym_COMMA] = ACTIONS(580),
  },
  [497] = {
    [anon_sym_where] = ACTIONS(745),
    [anon_sym_RBRACE] = ACTIONS(745),
    [anon_sym_COMMA] = ACTIONS(745),
    [sym_comment] = ACTIONS(3),
    [anon_sym_EQ] = ACTIONS(745),
    [anon_sym_SEMI] = ACTIONS(745),
  },
  [498] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(253),
    [sym_type] = STATE(459),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(397),
    [anon_sym_LPAREN] = ACTIONS(399),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(401),
  },
  [499] = {
    [sym_record_type] = STATE(77),
    [sym_path] = STATE(261),
    [sym_type] = STATE(459),
    [sym_function_type] = STATE(77),
    [sym_unit_type] = STATE(77),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(117),
    [anon_sym_DOT] = ACTIONS(409),
    [anon_sym_LPAREN] = ACTIONS(411),
    [anon_sym_LBRACE] = ACTIONS(123),
    [sym_identifier] = ACTIONS(413),
  },
  [500] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(282),
    [sym_type] = STATE(497),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(447),
    [anon_sym_LPAREN] = ACTIONS(449),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(451),
  },
  [501] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(777),
  },
  [502] = {
    [anon_sym_in] = ACTIONS(568),
    [sym_comment] = ACTIONS(3),
  },
  [503] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(309),
    [sym_type] = STATE(497),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(507),
    [anon_sym_LPAREN] = ACTIONS(509),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(511),
  },
  [504] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_RPAREN] = ACTIONS(779),
  },
  [505] = {
    [sym_record_type] = STATE(319),
    [sym_path] = STATE(320),
    [sym_type] = STATE(509),
    [sym_function_type] = STATE(319),
    [sym_unit_type] = STATE(319),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(522),
    [anon_sym_DOT] = ACTIONS(524),
    [anon_sym_LPAREN] = ACTIONS(526),
    [anon_sym_LBRACE] = ACTIONS(528),
    [sym_identifier] = ACTIONS(530),
  },
  [506] = {
    [sym__align] = ACTIONS(709),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(709),
  },
  [507] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(781),
  },
  [508] = {
    [sym_comment] = ACTIONS(3),
    [anon_sym_DASH_GT] = ACTIONS(783),
  },
  [509] = {
    [sym__align] = ACTIONS(745),
    [sym_comment] = ACTIONS(3),
    [sym__dedent] = ACTIONS(745),
  },
  [510] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(420),
    [sym_type] = STATE(497),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(659),
    [anon_sym_LPAREN] = ACTIONS(661),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(663),
  },
  [511] = {
    [sym_record_type] = STATE(174),
    [sym_path] = STATE(440),
    [sym_type] = STATE(497),
    [sym_function_type] = STATE(174),
    [sym_unit_type] = STATE(174),
    [sym_comment] = ACTIONS(3),
    [anon_sym_LPAREN_RPAREN] = ACTIONS(286),
    [anon_sym_DOT] = ACTIONS(689),
    [anon_sym_LPAREN] = ACTIONS(691),
    [anon_sym_LBRACE] = ACTIONS(292),
    [sym_identifier] = ACTIONS(693),
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
  [27] = {.count = 1, .reusable = true}, SHIFT(24),
  [29] = {.count = 1, .reusable = true}, REDUCE(sym_definition, 1),
  [31] = {.count = 1, .reusable = true},  ACCEPT_INPUT(),
  [33] = {.count = 1, .reusable = true}, SHIFT(26),
  [35] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 2),
  [37] = {.count = 1, .reusable = true}, SHIFT(28),
  [39] = {.count = 1, .reusable = true}, REDUCE(sym_atomic_expression, 1),
  [41] = {.count = 1, .reusable = true}, REDUCE(sym_unit, 1),
  [43] = {.count = 1, .reusable = true}, SHIFT(30),
  [45] = {.count = 1, .reusable = true}, SHIFT(31),
  [47] = {.count = 1, .reusable = true}, SHIFT(32),
  [49] = {.count = 1, .reusable = true}, SHIFT(33),
  [51] = {.count = 1, .reusable = false}, SHIFT(34),
  [53] = {.count = 1, .reusable = false}, SHIFT(35),
  [55] = {.count = 1, .reusable = true}, SHIFT(40),
  [57] = {.count = 1, .reusable = true}, REDUCE(sym_path, 1),
  [59] = {.count = 1, .reusable = true}, REDUCE(sym_binding, 1),
  [61] = {.count = 1, .reusable = true}, REDUCE(sym_pattern, 1),
  [63] = {.count = 1, .reusable = true}, REDUCE(sym_atomic_pattern, 1),
  [65] = {.count = 1, .reusable = true}, REDUCE(sym_application_expression, 1),
  [67] = {.count = 1, .reusable = true}, REDUCE(sym_record_pattern, 1),
  [69] = {.count = 1, .reusable = true}, SHIFT(43),
  [71] = {.count = 1, .reusable = true}, SHIFT(45),
  [73] = {.count = 1, .reusable = true}, REDUCE(sym_application_pattern, 1),
  [75] = {.count = 1, .reusable = false}, SHIFT(46),
  [77] = {.count = 1, .reusable = true}, SHIFT(47),
  [79] = {.count = 1, .reusable = true}, SHIFT(48),
  [81] = {.count = 1, .reusable = false}, SHIFT(50),
  [83] = {.count = 1, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2),
  [85] = {.count = 2, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(54),
  [88] = {.count = 2, .reusable = true}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(53),
  [91] = {.count = 1, .reusable = true}, REDUCE(sym_source_file, 3),
  [93] = {.count = 1, .reusable = true}, SHIFT(56),
  [95] = {.count = 1, .reusable = true}, REDUCE(sym_path, 2),
  [97] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 2),
  [99] = {.count = 1, .reusable = true}, SHIFT(59),
  [101] = {.count = 1, .reusable = true}, SHIFT(60),
  [103] = {.count = 1, .reusable = true}, SHIFT(62),
  [105] = {.count = 1, .reusable = true}, SHIFT(61),
  [107] = {.count = 1, .reusable = true}, SHIFT(65),
  [109] = {.count = 1, .reusable = true}, SHIFT(66),
  [111] = {.count = 1, .reusable = true}, SHIFT(69),
  [113] = {.count = 1, .reusable = true}, SHIFT(70),
  [115] = {.count = 1, .reusable = true}, SHIFT(71),
  [117] = {.count = 1, .reusable = true}, SHIFT(72),
  [119] = {.count = 1, .reusable = true}, SHIFT(73),
  [121] = {.count = 1, .reusable = false}, SHIFT(74),
  [123] = {.count = 1, .reusable = false}, SHIFT(75),
  [125] = {.count = 1, .reusable = true}, SHIFT(76),
  [127] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(14),
  [130] = {.count = 1, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2),
  [132] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(28),
  [135] = {.count = 1, .reusable = true}, SHIFT(83),
  [137] = {.count = 1, .reusable = false}, SHIFT(81),
  [139] = {.count = 1, .reusable = true}, SHIFT(82),
  [141] = {.count = 1, .reusable = true}, SHIFT(84),
  [143] = {.count = 1, .reusable = false}, SHIFT(85),
  [145] = {.count = 1, .reusable = false}, SHIFT(86),
  [147] = {.count = 1, .reusable = false}, SHIFT(87),
  [149] = {.count = 1, .reusable = true}, REDUCE(sym_parameter_list, 1),
  [151] = {.count = 1, .reusable = true}, SHIFT(91),
  [153] = {.count = 1, .reusable = true}, SHIFT(93),
  [155] = {.count = 1, .reusable = false}, SHIFT(92),
  [157] = {.count = 1, .reusable = true}, SHIFT(94),
  [159] = {.count = 1, .reusable = false}, SHIFT(95),
  [161] = {.count = 1, .reusable = true}, SHIFT(96),
  [163] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 3),
  [165] = {.count = 1, .reusable = false}, REDUCE(sym_parameter_list, 1),
  [167] = {.count = 1, .reusable = false}, SHIFT(100),
  [169] = {.count = 1, .reusable = true}, SHIFT(46),
  [171] = {.count = 1, .reusable = true}, SHIFT(101),
  [173] = {.count = 1, .reusable = true}, REDUCE(sym_type_definition_rhs, 1),
  [175] = {.count = 1, .reusable = false}, SHIFT(103),
  [177] = {.count = 1, .reusable = false}, SHIFT(104),
  [179] = {.count = 1, .reusable = true}, SHIFT(106),
  [181] = {.count = 1, .reusable = true}, SHIFT(108),
  [183] = {.count = 1, .reusable = true}, REDUCE(sym_path, 3),
  [185] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression_entry, 2),
  [187] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 3),
  [189] = {.count = 1, .reusable = true}, SHIFT(110),
  [191] = {.count = 1, .reusable = true}, SHIFT(112),
  [193] = {.count = 1, .reusable = false}, SHIFT(113),
  [195] = {.count = 1, .reusable = true}, SHIFT(114),
  [197] = {.count = 1, .reusable = false}, SHIFT(115),
  [199] = {.count = 1, .reusable = false}, SHIFT(116),
  [201] = {.count = 1, .reusable = false}, SHIFT(117),
  [203] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2),
  [205] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2), SHIFT_REPEAT(32),
  [208] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_expression_repeat1, 2), SHIFT_REPEAT(122),
  [211] = {.count = 1, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2),
  [213] = {.count = 1, .reusable = true}, REDUCE(sym_unit_type, 1),
  [215] = {.count = 1, .reusable = true}, SHIFT(125),
  [217] = {.count = 1, .reusable = true}, SHIFT(126),
  [219] = {.count = 1, .reusable = true}, SHIFT(127),
  [221] = {.count = 1, .reusable = true}, SHIFT(129),
  [223] = {.count = 1, .reusable = true}, SHIFT(130),
  [225] = {.count = 1, .reusable = true}, SHIFT(133),
  [227] = {.count = 1, .reusable = true}, REDUCE(sym_type, 1),
  [229] = {.count = 1, .reusable = true}, SHIFT(135),
  [231] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_signature, 4),
  [233] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(39),
  [236] = {.count = 1, .reusable = false}, SHIFT(140),
  [238] = {.count = 1, .reusable = true}, SHIFT(137),
  [240] = {.count = 1, .reusable = true}, SHIFT(138),
  [242] = {.count = 1, .reusable = false}, SHIFT(136),
  [244] = {.count = 1, .reusable = false}, SHIFT(139),
  [246] = {.count = 1, .reusable = true}, SHIFT(146),
  [248] = {.count = 1, .reusable = true}, SHIFT(147),
  [250] = {.count = 1, .reusable = false}, SHIFT(148),
  [252] = {.count = 1, .reusable = false}, SHIFT(149),
  [254] = {.count = 1, .reusable = true}, REDUCE(sym_expression, 1),
  [256] = {.count = 1, .reusable = true}, SHIFT(87),
  [258] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 4),
  [260] = {.count = 1, .reusable = true}, REDUCE(aux_sym_parameter_list_repeat1, 2),
  [262] = {.count = 2, .reusable = true}, REDUCE(aux_sym_parameter_list_repeat1, 2), SHIFT_REPEAT(91),
  [265] = {.count = 1, .reusable = true}, SHIFT(156),
  [267] = {.count = 1, .reusable = true}, SHIFT(157),
  [269] = {.count = 1, .reusable = true}, SHIFT(160),
  [271] = {.count = 1, .reusable = true}, SHIFT(163),
  [273] = {.count = 1, .reusable = true}, SHIFT(165),
  [275] = {.count = 1, .reusable = true}, SHIFT(167),
  [277] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_signature, 4),
  [279] = {.count = 1, .reusable = true}, REDUCE(sym_type_alias_definition_rhs, 2),
  [281] = {.count = 1, .reusable = false}, REDUCE(aux_sym_parameter_list_repeat1, 2),
  [283] = {.count = 2, .reusable = false}, REDUCE(aux_sym_parameter_list_repeat1, 2), SHIFT_REPEAT(100),
  [286] = {.count = 1, .reusable = true}, SHIFT(169),
  [288] = {.count = 1, .reusable = true}, SHIFT(170),
  [290] = {.count = 1, .reusable = false}, SHIFT(171),
  [292] = {.count = 1, .reusable = false}, SHIFT(172),
  [294] = {.count = 1, .reusable = true}, SHIFT(173),
  [296] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 4),
  [298] = {.count = 1, .reusable = true}, SHIFT(178),
  [300] = {.count = 1, .reusable = true}, SHIFT(179),
  [302] = {.count = 1, .reusable = true}, SHIFT(181),
  [304] = {.count = 1, .reusable = true}, SHIFT(182),
  [306] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 5),
  [308] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 4),
  [310] = {.count = 1, .reusable = true}, SHIFT(185),
  [312] = {.count = 1, .reusable = true}, SHIFT(188),
  [314] = {.count = 1, .reusable = true}, SHIFT(117),
  [316] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression_entry, 3),
  [318] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(65),
  [321] = {.count = 1, .reusable = true}, SHIFT(193),
  [323] = {.count = 1, .reusable = true}, SHIFT(196),
  [325] = {.count = 1, .reusable = true}, SHIFT(39),
  [327] = {.count = 1, .reusable = true}, SHIFT(198),
  [329] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 2),
  [331] = {.count = 1, .reusable = true}, REDUCE(sym_record_label, 1),
  [333] = {.count = 1, .reusable = true}, SHIFT(199),
  [335] = {.count = 1, .reusable = true}, SHIFT(200),
  [337] = {.count = 1, .reusable = true}, SHIFT(203),
  [339] = {.count = 1, .reusable = true}, SHIFT(206),
  [341] = {.count = 1, .reusable = true}, SHIFT(209),
  [343] = {.count = 1, .reusable = false}, REDUCE(sym_path, 1),
  [345] = {.count = 1, .reusable = true}, SHIFT(213),
  [347] = {.count = 1, .reusable = true}, SHIFT(214),
  [349] = {.count = 1, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 1),
  [351] = {.count = 1, .reusable = false}, SHIFT(216),
  [353] = {.count = 1, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 1),
  [355] = {.count = 1, .reusable = true}, SHIFT(217),
  [357] = {.count = 1, .reusable = true}, SHIFT(219),
  [359] = {.count = 1, .reusable = true}, SHIFT(149),
  [361] = {.count = 1, .reusable = true}, SHIFT(223),
  [363] = {.count = 1, .reusable = true}, SHIFT(224),
  [365] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(84),
  [368] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(87),
  [371] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 3),
  [373] = {.count = 1, .reusable = true}, SHIFT(226),
  [375] = {.count = 1, .reusable = true}, SHIFT(227),
  [377] = {.count = 1, .reusable = true}, SHIFT(230),
  [379] = {.count = 1, .reusable = true}, SHIFT(231),
  [381] = {.count = 1, .reusable = true}, SHIFT(234),
  [383] = {.count = 1, .reusable = true}, SHIFT(237),
  [385] = {.count = 1, .reusable = true}, SHIFT(238),
  [387] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 5),
  [389] = {.count = 1, .reusable = true}, SHIFT(240),
  [391] = {.count = 1, .reusable = true}, SHIFT(242),
  [393] = {.count = 1, .reusable = true}, SHIFT(246),
  [395] = {.count = 1, .reusable = true}, SHIFT(248),
  [397] = {.count = 1, .reusable = true}, SHIFT(250),
  [399] = {.count = 1, .reusable = false}, SHIFT(251),
  [401] = {.count = 1, .reusable = true}, SHIFT(252),
  [403] = {.count = 1, .reusable = true}, SHIFT(254),
  [405] = {.count = 1, .reusable = false}, SHIFT(255),
  [407] = {.count = 1, .reusable = false}, SHIFT(256),
  [409] = {.count = 1, .reusable = true}, SHIFT(258),
  [411] = {.count = 1, .reusable = false}, SHIFT(259),
  [413] = {.count = 1, .reusable = true}, SHIFT(260),
  [415] = {.count = 1, .reusable = true}, SHIFT(263),
  [417] = {.count = 1, .reusable = true}, REDUCE(sym_top_value_definition, 6),
  [419] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 5),
  [421] = {.count = 1, .reusable = false}, SHIFT(70),
  [423] = {.count = 1, .reusable = true}, SHIFT(264),
  [425] = {.count = 1, .reusable = true}, SHIFT(266),
  [427] = {.count = 1, .reusable = true}, SHIFT(267),
  [429] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(114),
  [432] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(117),
  [435] = {.count = 1, .reusable = true}, SHIFT(269),
  [437] = {.count = 1, .reusable = false}, SHIFT(270),
  [439] = {.count = 1, .reusable = false}, SHIFT(271),
  [441] = {.count = 1, .reusable = true}, SHIFT(273),
  [443] = {.count = 1, .reusable = true}, SHIFT(276),
  [445] = {.count = 1, .reusable = true}, SHIFT(277),
  [447] = {.count = 1, .reusable = true}, SHIFT(279),
  [449] = {.count = 1, .reusable = false}, SHIFT(280),
  [451] = {.count = 1, .reusable = true}, SHIFT(281),
  [453] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 3),
  [455] = {.count = 1, .reusable = true}, SHIFT(284),
  [457] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 2),
  [459] = {.count = 2, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 2), SHIFT_REPEAT(130),
  [462] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(133),
  [465] = {.count = 1, .reusable = true}, REDUCE(sym_function_type, 3),
  [467] = {.count = 1, .reusable = true}, SHIFT(286),
  [469] = {.count = 1, .reusable = false}, REDUCE(sym_path, 2),
  [471] = {.count = 1, .reusable = true}, SHIFT(289),
  [473] = {.count = 1, .reusable = true}, SHIFT(290),
  [475] = {.count = 1, .reusable = true}, SHIFT(292),
  [477] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(213),
  [480] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(214),
  [483] = {.count = 1, .reusable = true}, REDUCE(sym_suspension, 3),
  [485] = {.count = 1, .reusable = true}, SHIFT(296),
  [487] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(147),
  [490] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(149),
  [493] = {.count = 1, .reusable = true}, REDUCE(sym_expression, 3),
  [495] = {.count = 1, .reusable = true}, SHIFT(300),
  [497] = {.count = 1, .reusable = false}, SHIFT(298),
  [499] = {.count = 1, .reusable = true}, SHIFT(299),
  [501] = {.count = 1, .reusable = true}, SHIFT(301),
  [503] = {.count = 1, .reusable = false}, SHIFT(302),
  [505] = {.count = 1, .reusable = false}, SHIFT(303),
  [507] = {.count = 1, .reusable = true}, SHIFT(306),
  [509] = {.count = 1, .reusable = false}, SHIFT(307),
  [511] = {.count = 1, .reusable = true}, SHIFT(308),
  [513] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 4),
  [515] = {.count = 1, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat1, 2),
  [517] = {.count = 2, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat1, 2), SHIFT_REPEAT(311),
  [520] = {.count = 1, .reusable = true}, SHIFT(313),
  [522] = {.count = 1, .reusable = true}, SHIFT(314),
  [524] = {.count = 1, .reusable = true}, SHIFT(315),
  [526] = {.count = 1, .reusable = false}, SHIFT(316),
  [528] = {.count = 1, .reusable = false}, SHIFT(317),
  [530] = {.count = 1, .reusable = true}, SHIFT(318),
  [532] = {.count = 1, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat2, 2),
  [534] = {.count = 2, .reusable = true}, REDUCE(aux_sym_data_type_definition_rhs_repeat2, 2), SHIFT_REPEAT(322),
  [537] = {.count = 1, .reusable = true}, REDUCE(sym__semi, 1, .alias_sequence_id = 1),
  [539] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(165),
  [542] = {.count = 1, .reusable = true}, SHIFT(326),
  [544] = {.count = 1, .reusable = true}, SHIFT(327),
  [546] = {.count = 1, .reusable = true}, SHIFT(328),
  [548] = {.count = 1, .reusable = true}, REDUCE(sym_top_type_definition, 6),
  [550] = {.count = 1, .reusable = true}, SHIFT(332),
  [552] = {.count = 1, .reusable = true}, SHIFT(333),
  [554] = {.count = 1, .reusable = true}, SHIFT(336),
  [556] = {.count = 1, .reusable = true}, SHIFT(337),
  [558] = {.count = 1, .reusable = true}, SHIFT(256),
  [560] = {.count = 1, .reusable = true}, SHIFT(341),
  [562] = {.count = 1, .reusable = true}, SHIFT(344),
  [564] = {.count = 1, .reusable = true}, SHIFT(347),
  [566] = {.count = 1, .reusable = true}, SHIFT(271),
  [568] = {.count = 1, .reusable = true}, REDUCE(sym_record_expression, 6),
  [570] = {.count = 1, .reusable = true}, SHIFT(351),
  [572] = {.count = 1, .reusable = true}, SHIFT(353),
  [574] = {.count = 1, .reusable = true}, SHIFT(354),
  [576] = {.count = 1, .reusable = true}, SHIFT(357),
  [578] = {.count = 1, .reusable = true}, SHIFT(358),
  [580] = {.count = 1, .reusable = true}, SHIFT(359),
  [582] = {.count = 1, .reusable = true}, SHIFT(361),
  [584] = {.count = 1, .reusable = true}, SHIFT(362),
  [586] = {.count = 1, .reusable = false}, REDUCE(sym_path, 3),
  [588] = {.count = 1, .reusable = false}, REDUCE(aux_sym_path_repeat1, 2),
  [590] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(211),
  [593] = {.count = 1, .reusable = true}, SHIFT(369),
  [595] = {.count = 1, .reusable = false}, SHIFT(368),
  [597] = {.count = 1, .reusable = true}, SHIFT(373),
  [599] = {.count = 1, .reusable = false}, REDUCE(sym_application_expression, 1),
  [601] = {.count = 1, .reusable = true}, SHIFT(378),
  [603] = {.count = 1, .reusable = true}, SHIFT(379),
  [605] = {.count = 1, .reusable = true}, SHIFT(382),
  [607] = {.count = 1, .reusable = true}, REDUCE(sym_data_constructor_definition, 3),
  [609] = {.count = 1, .reusable = true}, SHIFT(383),
  [611] = {.count = 1, .reusable = true}, REDUCE(sym_data_type_definition_rhs, 5),
  [613] = {.count = 1, .reusable = true}, SHIFT(384),
  [615] = {.count = 1, .reusable = true}, SHIFT(386),
  [617] = {.count = 1, .reusable = true}, SHIFT(389),
  [619] = {.count = 1, .reusable = true}, SHIFT(391),
  [621] = {.count = 1, .reusable = true}, SHIFT(392),
  [623] = {.count = 1, .reusable = true}, SHIFT(393),
  [625] = {.count = 1, .reusable = true}, SHIFT(396),
  [627] = {.count = 1, .reusable = true}, SHIFT(398),
  [629] = {.count = 1, .reusable = true}, SHIFT(401),
  [631] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(254),
  [634] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(256),
  [637] = {.count = 1, .reusable = true}, SHIFT(404),
  [639] = {.count = 1, .reusable = true}, SHIFT(406),
  [641] = {.count = 1, .reusable = true}, SHIFT(408),
  [643] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(269),
  [646] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(271),
  [649] = {.count = 1, .reusable = true}, SHIFT(412),
  [651] = {.count = 1, .reusable = true}, SHIFT(414),
  [653] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 5),
  [655] = {.count = 1, .reusable = true}, REDUCE(aux_sym_record_type_repeat1, 4),
  [657] = {.count = 1, .reusable = true}, SHIFT(416),
  [659] = {.count = 1, .reusable = true}, SHIFT(417),
  [661] = {.count = 1, .reusable = false}, SHIFT(418),
  [663] = {.count = 1, .reusable = true}, SHIFT(419),
  [665] = {.count = 1, .reusable = true}, SHIFT(422),
  [667] = {.count = 1, .reusable = true}, SHIFT(423),
  [669] = {.count = 1, .reusable = true}, SHIFT(424),
  [671] = {.count = 1, .reusable = true}, SHIFT(425),
  [673] = {.count = 1, .reusable = false}, SHIFT(428),
  [675] = {.count = 1, .reusable = true}, SHIFT(430),
  [677] = {.count = 1, .reusable = true}, SHIFT(431),
  [679] = {.count = 2, .reusable = true}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(301),
  [682] = {.count = 1, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2),
  [684] = {.count = 2, .reusable = false}, REDUCE(aux_sym_application_expression_repeat1, 2), SHIFT_REPEAT(303),
  [687] = {.count = 1, .reusable = true}, SHIFT(435),
  [689] = {.count = 1, .reusable = true}, SHIFT(437),
  [691] = {.count = 1, .reusable = false}, SHIFT(438),
  [693] = {.count = 1, .reusable = true}, SHIFT(439),
  [695] = {.count = 1, .reusable = true}, SHIFT(442),
  [697] = {.count = 1, .reusable = true}, SHIFT(443),
  [699] = {.count = 1, .reusable = true}, SHIFT(444),
  [701] = {.count = 1, .reusable = true}, SHIFT(446),
  [703] = {.count = 1, .reusable = true}, SHIFT(450),
  [705] = {.count = 1, .reusable = true}, SHIFT(451),
  [707] = {.count = 1, .reusable = true}, SHIFT(452),
  [709] = {.count = 1, .reusable = true}, REDUCE(sym_record_type, 6),
  [711] = {.count = 1, .reusable = true}, SHIFT(461),
  [713] = {.count = 1, .reusable = true}, SHIFT(464),
  [715] = {.count = 1, .reusable = false}, SHIFT(214),
  [717] = {.count = 1, .reusable = true}, SHIFT(465),
  [719] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(375),
  [722] = {.count = 1, .reusable = true}, REDUCE(sym_let_binding, 6),
  [724] = {.count = 1, .reusable = true}, SHIFT(470),
  [726] = {.count = 1, .reusable = true}, SHIFT(473),
  [728] = {.count = 1, .reusable = true}, SHIFT(476),
  [730] = {.count = 2, .reusable = true}, REDUCE(aux_sym_path_repeat1, 2), SHIFT_REPEAT(389),
  [733] = {.count = 1, .reusable = true}, SHIFT(477),
  [735] = {.count = 1, .reusable = true}, SHIFT(478),
  [737] = {.count = 1, .reusable = true}, SHIFT(479),
  [739] = {.count = 1, .reusable = true}, SHIFT(480),
  [741] = {.count = 1, .reusable = true}, SHIFT(481),
  [743] = {.count = 1, .reusable = true}, SHIFT(482),
  [745] = {.count = 1, .reusable = true}, REDUCE(sym_function_type, 7),
  [747] = {.count = 1, .reusable = true}, SHIFT(483),
  [749] = {.count = 1, .reusable = true}, SHIFT(485),
  [751] = {.count = 1, .reusable = true}, SHIFT(487),
  [753] = {.count = 1, .reusable = true}, SHIFT(489),
  [755] = {.count = 1, .reusable = true}, SHIFT(490),
  [757] = {.count = 1, .reusable = true}, SHIFT(492),
  [759] = {.count = 1, .reusable = true}, SHIFT(494),
  [761] = {.count = 1, .reusable = true}, SHIFT(495),
  [763] = {.count = 1, .reusable = true}, SHIFT(498),
  [765] = {.count = 1, .reusable = true}, SHIFT(499),
  [767] = {.count = 1, .reusable = true}, SHIFT(500),
  [769] = {.count = 1, .reusable = true}, SHIFT(502),
  [771] = {.count = 1, .reusable = true}, SHIFT(503),
  [773] = {.count = 1, .reusable = true}, SHIFT(505),
  [775] = {.count = 1, .reusable = true}, SHIFT(506),
  [777] = {.count = 1, .reusable = true}, SHIFT(507),
  [779] = {.count = 1, .reusable = true}, SHIFT(508),
  [781] = {.count = 1, .reusable = true}, SHIFT(510),
  [783] = {.count = 1, .reusable = true}, SHIFT(511),
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
