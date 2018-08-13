/// All predicates return false for the `FAUX_CHAR`.

pub fn contains<T: Eq + Copy>(a: &[T], x: T) -> bool {
    a.iter().find(|&&z| z == x).is_some()
}

/// Rust is opinionated enough to disallow surrogates inside char. E.g. the
/// char literal '\u{d800}' is disallowed.
/// So we use a private area character.
pub const FAUX_CHAR: char = '\u{f8ff}';

pub fn is_space(c: char) -> bool {
    c == ' '
}

pub const ARROW_HEAD_CHARS: [char; 4] = ['>', 'v', '<', '^'];

pub fn is_arrow_head(c: char) -> bool {
    contains(&ARROW_HEAD_CHARS, c)
}

pub const POINT_CHARS: [char; 2] = ['o', '*'];

pub fn is_point(c: char) -> bool {
    contains(&POINT_CHARS, c)
}

// NOTE: Since Rust doesn't have static array concatenation, we manually inline
// the arrays here. Since we are trying to stick to what Markdeep does, there
// is a test which fails if this definition is changed.
pub const DECORATION_CHARS: [char; 17] = [
    // ARROW_HEAD_CHARS
    '>', 'v', '<', '^',
    // POINT_CHARS
    'o', '*',
    // JUMP_CHARS
    '(', ')',
    // GRAY_CHARS
    '\u{2591}', '\u{2592}', '\u{2593}', '\u{2594}', '\u{2589}',
    // TRI_CHARS
    '\u{25E2}', '\u{25E3}', '\u{25E4}', '\u{25E5}'
];

pub fn is_decoration(c: char) -> bool {
    contains(&DECORATION_CHARS, c)
}

pub const JUMP_CHARS: [char; 2] = ['(', ')'];

pub fn is_jump(c: char) -> bool {
    contains(&JUMP_CHARS, c)
}

pub const UNDIRECTED_VERTEX_CHARS: [char; 1] = ['+'];

pub fn is_undirected_vertex(c: char) -> bool {
    c == '+'
}

pub const VERTEX_CHARS: [char; 3] = ['+', '.', '\''];

pub fn is_vertex(c: char) -> bool {
    contains(&VERTEX_CHARS, c)
}

pub fn is_top_vertex(c: char) -> bool {
    is_undirected_vertex(c) || c == '.'
}

pub fn is_bot_vertex(c: char) -> bool {
    is_undirected_vertex(c) || c == '\''
}

pub fn is_vertex_or_left_decoration(c: char) -> bool {
    is_vertex(c) || c == '<' || is_point(c)
}

pub fn is_vertex_or_right_decoration(c: char) -> bool {
    is_vertex(c) || c == '>' || is_point(c)
}

// GRAY[i] is the Unicode block character for (i+1)/4 level gray
pub const GRAY_CHARS: [char; 5] =
    ['\u{2591}', '\u{2592}', '\u{2593}', '\u{2594}', '\u{2589}'];

pub fn is_gray(c: char) -> bool {
    contains(&GRAY_CHARS, c)
}

// TRI[i] is a right-triangle rotated by 90*i
pub const TRI_CHARS: [char; 4] =
    ['\u{25E2}', '\u{25E3}', '\u{25E4}', '\u{25E5}'];

pub fn is_tri(c: char) -> bool {
    contains(&TRI_CHARS, c)
}

pub fn is_solid_hline(c: char) -> bool {
    c == '-' || is_undirected_vertex(c) || is_jump(c)
}

pub fn is_solid_vline_or_jump_or_point(c: char) -> bool {
    is_solid_vline(c) || is_jump(c) || is_point(c)
}

pub fn is_solid_vline(c: char) -> bool {
    c == '|' || is_undirected_vertex(c)
}

pub fn is_solid_dline(c: char) -> bool {
    c == '/' || is_undirected_vertex(c)
}

pub fn is_solid_bline(c: char) -> bool {
    c == '\\' || is_undirected_vertex(c)
}

#[test]
fn decoration_chars_definition() {
    let it = ARROW_HEAD_CHARS.iter().chain(&POINT_CHARS).chain(&JUMP_CHARS)
        .chain(&GRAY_CHARS).chain(&TRI_CHARS);
    for (i, &c) in it.enumerate() {
        assert!(DECORATION_CHARS[i] == c);
    }
}
