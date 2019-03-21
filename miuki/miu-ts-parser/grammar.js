module.exports = grammar({
    name: 'miu',

    rules: {
        // The production rules of the context-free grammar
        source_file: $ => repeat($._definition),

        _definition: $ => choice(
            $.top_value_signature,
            $.top_value_definition,
            $.top_type_signature,
            $.top_type_definition
        ),

        top_value_signature: $ => seq(
            'let',
            $.identifier,
            ':',
            $.type_signature
        ),

        top_value_definition: $ => seq(
            'let',
            $.identifier,
            optional($.parameter_list),
            '=',
            $.expression
        ),

        expression: $ => choice(
            $.application
        ),

        application: $ => repeat1($.path),

        path: $ => seq($.identifier, repeat(seq('.', $.identifier))),

        top_type_signature: $ => seq(
            'type',
            $.identifier,
            ':',
            $.type_signature
        ),

        top_type_definition: $ => seq(
            'type',
            $.identifier,
            optional($.parameter_list),
            optional(seq(':', $.type_signature)),
            $.type_definition_rhs
        ),

        type_definition_rhs: $ => choice(
            $.data_type_definition_rhs,
            $.type_alias_definition_rhs
        ),

        data_type_definition_rhs: $ => seq(
            'where',
            '{',
            repeat(seq($.data_constructor_definition, ';')),
            '}'
        ),

        type_alias_definition_rhs: $ => seq(
            '=',
            $.type_signature
        ),

        data_constructor_definition: $ => seq(
            $.identifier,
            ':',
            $.type_signature
        ),

        identifier: $ => /[A-Za-z]+/,

        type_signature: $ => seq(
            $.identifier,
            repeat(seq('->', $.identifier))
        ),

        parameter_list: $ => repeat1($.identifier)
    }
});
