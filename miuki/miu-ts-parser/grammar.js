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

    rules: {

        source_file: $ => repeat($.definition),

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

        non_newline_whitespace: $ =>
            /[ \f\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]/,

        definition: $ => choice(
            $.top_value_signature,
            $.top_value_definition,
            $.top_type_signature,
            $.top_type_definition
        ),

        top_value_signature: $ => seq(
            'let',
            $.identifier,
            ':',
            $.type
        ),

        top_value_definition: $ => seq(
            'let',
            optional('rec'),
            $.binding,
            optional($.parameter_list),
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

        identifier: $ => /[A-Za-z][A-Za-z0-9]*/,

        parameter_list: $ => repeat1($.identifier),

        path: $ => prec.left(1, seq(
            // First '.' may be used for more ergonomic field updates,
            // amongst other things.
            optional('.'),
            sepBy1($.identifier, '.')
        )),

        type: $ => choice(
            $.path,
            $.unit_type,
            $.record_type,
            $.function_type,
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

        record_label: $ => $.identifier,

        expression: $ => choice(
            $.atomic_expression,
            $.application_expression,
            seq('(', $.expression , ')'),
            $.suspension,
            $.record_expression,
            $.let_binding,
        ),

        binding: $ => $.pattern,

        pattern: $ => choice(
            $.atomic_pattern,
            // Note: The following can actually have two distinct meanings.
            // In the context of bindings, if we have
            // let f a b c =, that is allowed. However, in the context of
            // case .. of { f a b c -> ; }, we should return an error later on.
            //
            // This case is a bit of a PITA because all paths might possibly be
            // constructors (ignoring case restrictions) and all identifiers
            // are paths too.
            $.application_pattern,
            $.record_pattern,
        ),

        let_binding: $ => seq(
            'let',
            $.binding,
            '=',
            $.expression,
            'in',
            $.expression
        ),

        atomic_expression: $ => choice(
            $.unit,
            $.integer
        ),

        atomic_pattern: $ => $.atomic_expression,

        unit: $ => '()',

        integer: $ => /[0-9](_?[0-9]*)/,

        // This is terrible :(
        application_expression: $ => prec.left(1, repeat1($.path)),

        application_pattern: $ => $.application_expression,

        suspension: $ => seq('{', $.expression, '}'),

        record_expression: $ => seq(
            '{',
            optional(seq(optional($.path), 'with')),
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
            '{',
            sepEndBy($.data_constructor_definition, ';'),
            '}'
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
