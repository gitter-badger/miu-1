// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// IMPORTANT: The names being used here are coordinated with the processing
// in the syntax::abstract module. So if you change any names or structure here,
// make sure to update that file as well.

module.exports = grammar({
    name: 'miu',

    extras: $ => [
        $.comment,
        // Copied from tree-sitter-python
        // Not entirely sure what this regex is doing.
        // Why is there a separate backslash?
        // Why are we handling \r\n here?
        /[\s\uFEFF\u2060\u200B]|\\\r?\n/
    ],

    externals: $ => [
        $._align,
        $._indent,
        $._dedent
    ],

    rules: {

        // It isn't clear to me where _align should be put here or not.
        source_file: $ => seq(
            optional($._align),
            sepEndBy($.definition, $._align)
        ),

        _begin: $ => alias($._indent, '_begin'),
        _end: $ => alias($._dedent, '_end'),
        _in: $ => alias($._align, '_in'),
        _semi: $ => alias($._align, '_semi'),

        comment: $ => token(choice(
            // TODO: This comment syntax isn't quite right, but tree-sitter
            // doesn't support start-of-line/end-of-line assertions (they would
            // anyways get messed up by the scanner), so I'm not sure how to handle
            // comments correctly. As it stands, we can't have operators which
            // have '--' in them.
            seq('--', /.*/),
            // No nesting.
            seq('{-', /(.|\n)*/, '-}')
        )),

        // TODO: Is this rule useful or should we delete it?
        // non_newline_whitespace: $ =>
        //     /[ \f\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]/,

        definition: $ => choice(
            $.top_value_signature,
            $.top_value_definition,
            $.top_type_signature,
            $.top_type_definition
        ),

        top_value_signature: $ => seq(
            'let',
            // This is incorrect, it should technically be identifier, not path.
            // However, we use path instead, so that when tree-sitter sees
            // 'let x', it doesn't mark the second token as identifier. If the
            // token gets classified as identifier, then having a non-':'
            // token come up next would lead to an error.
            $.path,
            ':',
            $.type
        ),

        top_value_definition: $ => seq(
            'let',
            optional('rec'),
            $.binding,
            '=',
            $.expression
        ),

        top_type_signature: $ => seq(
            'type',
            $.identifier,
            ':',
            $.type
        ),

        top_type_definition: $ => seq(
            'type',
            $.identifier,
            optional($.parameter_list),
            optional(seq(':', $.type)),
            $.type_definition_rhs
        ),

        identifier: $ => identifierRegex(),

        parameter_list: $ => repeat1($.identifier),

        // NOTE: Since we allow whitespaces everywhere, tree-sitter ends up
        path: $ => pathRegex(),

        // See the spec document for why extended paths are present.
        extended_path: $ => extendedPathRegex(),

        type: $ => choice(
            $.path,
            $.unit_type,
            $.record_type,
            $.function_type
        ),

        unit_type: $ => '()',

        record_type: $ => seq(
            '{',
            sepEndBy(seq($.record_label, ':', $.type), ','),
            '}'
        ),

        function_type: $ => choice(
            seq('(', $.path, ':', $.path, ')', '->', $.type),
            seq($.path, '->', $.type)
        ),

        record_label: $ => identifierRegex(),

        expression: $ => choice(
            $.extended_path,
            $.literal_expression,
            seq('(', $.expression , ')'),
            $.projection_expression,
            $.application_expression,
            $.suspension,
            $.record_expression,
            $.let_binding
        ),

        // This is too permissive, but we can check for errors later.
        binding: $ => $.pattern,

        pattern: $ => choice(
            $.literal_pattern,
            seq('(', $.pattern, ')'),
            // Note: The following can actually have two distinct meanings.
            // In the context of bindings, if we have
            // let f a b c =, that is allowed. However, in the context of
            // case .. of { f a b c -> ; }, we should return an error later on.
            //
            // This case is a bit of a PITA because all paths might possibly be
            // constructors (ignoring case restrictions) and all identifiers
            // are paths too.
            //
            // Since we don't distinguish between upper/lower case here, terms
            // with just 1 identifier also fall under eliminator_pattern.
            $.eliminator_pattern,
            $.record_pattern
        ),

        let_binding: $ => prec(1, seq(
            'let',
            optional('rec'),
            fsharpStyle($, seq($.binding, '=', $.expression)),
            'in',
            $.expression
        )),

        literal_expression: $ => choice(
            $.unit,
            $.integer,
            'True',
            'False',
            $.string,
        ),

        literal_pattern: $ => $.literal_expression,

        // Should we allow whitespace between the parens? Hmm...
        unit: $ => '()',

        integer: $ => /-?[0-9](_?[0-9]*)/,

        // Be maximally permissible here, returning errors later.
        string: $ => /"((\\")|[^"])*"/,

        projection_expression: $  => prec.left(13, seq($.expression, '.', $.identifier)),

        application_expression: $ => prec.left(11, seq(prec(2, repeat1($.expression)), $.expression)),

        // This is not sufficiently general :(
        eliminator_pattern: $ => prec.left(1, seq($.path, repeat($.pattern))),

        suspension: $ => seq('{', $.expression, '}'),

        record_expression: $ => seq(
            '{',
            optional(seq(optional($.expression), 'with')),
            sepEndBy($.record_expression_entry, ','),
            '}'
        ),

        record_expression_entry: $ => choice(
            // Stop-gap solution for field punning
            // The problem is the 'tilde-less' syntax conflicts with suspensions.
            // { foo } --> delay foo
            // { foo } --> { foo = foo }
            seq('~', $.identifier),
            seq($.identifier, '=', $.expression)
        ),

        record_pattern: $ => $.record_expression,

        type_definition_rhs: $ => choice(
            $.data_type_definition_rhs,
            $.type_alias_definition_rhs
        ),

        data_type_definition_rhs: $ => seq(
            'where',
            fsharpStyle($, $.data_constructor_definition)
        ),

        type_alias_definition_rhs: $ => seq(
            '=',
            $.type
        ),

        data_constructor_definition: $ => seq(
            $.identifier,
            ':',
            $.type
        )
    }
});

function sepBy1(rule, sep) {
    return seq(
        rule,
        repeat(seq(sep, rule))
    );
}

function sepEndBy(rule, sep) {
    return seq(
        repeat(seq(rule, sep)),
        optional(rule)
    );
}

function sepEndBy1(rule, sep) {
    return seq(
        rule,
        optional(seq(sep, sepEndBy(rule, sep)))
    );
}

function identifierRegex() {
    return /[A-Za-z][A-Za-z0-9]*/;
}

function pathRegex() {
    return new RegExp(identifierRegex().source + "(\\." + identifierRegex().source + ")*");
}

function extendedPathRegex() {
    return new RegExp("\\.?" + pathRegex().source);
}

function fsharpStyle($, rule) {
    return choice(
        seq(
            '{',
            sepEndBy(rule, ';'),
            '}'
        ), seq(
            $._begin,
            sepEndBy(rule, $._semi),
            $._end
        )
    );
}
