use diagram::primitives::*;

use unicode_width::UnicodeWidthChar;
use unicode_width::UnicodeWidthStr;

use std::cmp::max;
use std::iter;

pub struct V2 {
    pub x: usize,
    pub y: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MonoWidth(usize);

#[derive(Debug)]
pub struct Grid {
    /// Width according to a monospace font. This may or may not match the
    /// length of any String in data.
    pub width: MonoWidth,
    pub height: usize,
    data: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
/// In monospace fonts, Unicode assigns characters widths of 0, 1 or 2. So when
/// we are asked for "the character" (1 M wide) at a position (x, y), the
/// location may correspond to either the left or right of a 2-M wide char or
/// to a 1-M wide char.
pub enum CharMatch {
    Left,
    Exact,
    Right,
}

fn is_exact(c: CharMatch) -> bool {
    c == CharMatch::Exact
}

// NOTE: I use the suffix lf instead of lt (as in the `markdeep` source) so that
// there is less chance of mixing up lf and rt.
#[cfg_attr(rustfmt, rustfmt_skip)]
impl Grid {
    /// Basically `at_precise` but only considering `Exact` values.
    pub fn at(&self, x: usize, y: usize) -> Option<char> {
        match self.at_precise(x, y) {
            (Exact, c) => Some(c),
            _ => None,
        }
    }

    /// Basically `at_precise` but replacing 2-M wide chars with a
    /// Unicode surrogate characters for easy comparison failure
    /// with non-surrogate characters (which are legal in UTF-8).
    ///
    /// Use with care.
    pub fn at_faux(&self, x: usize, y: usize) -> char {
        match self.at_precise(x, y) {
            (Exact, c) => c,
            _ => FAUX_CHAR,
        }
    }

    /// The returned value is None whenever the corresponding "character" has
    /// width > 1. For example "ありがとう" will return `None` for
    /// `0 <= x < 10` and `y == 0`.
    /// Note that in the presence of zero-width characters, it may happen that
    /// the returned char doesn't fully incorporate what will be displayed.
    pub fn at_precise(&self, x: usize, y: usize) -> (CharMatch, char) {
        assert!(y < self.height);
        assert!(MonoWidth(x) < self.width);
        let mut tmp = 0;
        for c in self.data[y].chars() {
            let sz = match UnicodeWidthChar::width(c) {
                Some(a) => a,
                None => 0,
            };
            match sz {
                1 if (x == tmp)     => { return (CharMatch::Exact, c); }
                2 if (x == tmp)     => { return (CharMatch::Left , c); }
                2 if (x == tmp + 1) => { return (CharMatch::Right, c); }
                // 0 => (),
                _ => { tmp += sz; }
            }
        }
        unreachable!();
    }

    ///
    ///
    /// ```
    ///
    ///
    ///
    /// ```
    pub fn is_solid_vline_at(&self, x: usize, y: usize) -> bool {
        let up_lf = self.at_faux(x - 1, y - 1);
        let up    = self.at_faux(x    , y - 1);
        let up_rt = self.at_faux(x + 1, y - 1);
        let c     = self.at_faux(x    , y    );
        let dn_lf = self.at_faux(x - 1, y + 1);
        let dn    = self.at_faux(x    , y + 1);
        let dn_rt = self.at_faux(x + 1, y + 1);
        if is_solid_vline(c) {
            // Looks like a vertical line...does it continue?
            is_top_vertex(up) || up == '^' || is_solid_vline(up) || is_jump(up) ||
            is_bot_vertex(dn) || dn == 'v' || is_solid_vline(dn) || is_jump(dn) ||

            // TODO: Think whether dn_rt and dn_lf should be checked here.
            is_point(up) || is_point(dn) || up == '_' ||
            up_lf == '_' || up_rt == '_' ||

            // Special case of 1-high vertical on two curved corners
            ((is_top_vertex(up_lf) || is_top_vertex(up_rt)) &&
             (is_bot_vertex(dn_lf) || is_bot_vertex(dn_rt)))
        }
        else if is_top_vertex(c) || c == '^' {
            is_solid_vline(dn) || (is_jump(dn) && c != '.')
        }
        else if is_bot_vertex(c) || c == 'v' {
            is_solid_vline(up) || (is_jump(up) && c != '\'')
        }
        else if is_point(c) {
            is_solid_vline(up) || is_solid_vline(dn)
        }
        else {
            false
        }
    }

    ///
    ///
    /// ```
    ///
    ///
    ///
    /// ```
    pub fn is_solid_hline_at(&self, x: usize, y: usize) -> bool {
        let lf_lf = self.at_faux(x - 2, y);
        let lf    = self.at_faux(x - 1, y);
        let c     = self.at_faux(x    , y);
        let rt    = self.at_faux(x + 1, y);
        let rt_rt = self.at_faux(x + 2, y);

        if is_solid_hline(c) || (is_solid_hline(lf) && is_jump(c)) {
            // Looks like a horizontal line...does it continue? We need three in a row.
            if is_solid_hline(lf) {
                is_solid_hline(rt)    || is_vertex_or_right_decoration(rt) ||
                is_solid_hline(lf_lf) || is_vertex_or_left_decoration(lf_lf)
            } else if is_vertex_or_left_decoration(lf) {
                is_solid_hline(rt)
            } else {
                is_solid_hline(rt) &&
                (is_solid_hline(rt_rt) || is_vertex_or_right_decoration(rt_rt))
            }
        }
        else if c == '<' {
            is_solid_hline(rt) && is_solid_hline(rt_rt)
        }
        else if c == '>' {
            is_solid_hline(lf) && is_solid_hline(lf_lf)
        }
        else if is_vertex(c) {
            (is_solid_hline(lf) && is_solid_hline(lf_lf)) ||
            (is_solid_hline(rt) && is_solid_hline(rt_rt))
        }
        else {
            false
        }
    }

    ///
    ///
    /// ```
    ///
    ///
    ///
    /// ```
    pub fn is_solid_bline_at(&self, x: usize, y: usize) -> bool {
        let up_lf = self.at_faux(x - 1, y - 1);
        let up    = self.at_faux(x    , y - 1);
        let c     = self.at_faux(x    , y    );
        let dn    = self.at_faux(x    , y + 1);
        let dn_rt = self.at_faux(x + 1, y + 1);

        if (c == '\\') {
            // Looks like a diagonal line...does it continue? We need two in a row.
            is_solid_bline(dn_rt) || is_bot_vertex(dn_rt) || is_point(dn_rt) || dn_rt == 'v' ||
            is_solid_bline(up_lf) || is_top_vertex(up_lf) || is_point(up_lf) || up_lf == '^' ||
            up == '/' || dn == '/' || dn_rt == '_' || up_lf == '_'
        }
        else if c == '.' {
            dn_rt == '\\'
        }
        else if c == '\'' {
            up_lf == '\\'
        }
        else if c == '^' {
            dn_rt == '\\'
        }
        else if (c == 'v') {
            up_lf == '\\'
        }
        else if is_vertex(c) || is_point(c) || c == '|' {
            is_solid_bline(up_lf) || is_solid_bline(dn_rt)
        }
        else {
            false
        }
    }

    ///
    ///
    /// ```
    ///
    ///
    ///
    /// ```
    pub fn is_solid_dline_at(&self, x: usize, y: usize) -> bool {
        let up    = self.at_faux(x    , y - 1);
        let up_rt = self.at_faux(x + 1, y - 1);
        let c     = self.at_faux(x    , y    );
        let dn_lf = self.at_faux(x - 1, y + 1);
        let dn    = self.at_faux(x    , y + 1);

        if c == '/' && (up == '\\' || dn == '\\') {
            // Special case of tiny hexagon corner
            true
        }
        else if is_solid_dline(c) {
            // Looks like a diagonal line...does it continue? We need two in a row.
            is_solid_dline(up_rt) || is_top_vertex(up_rt) || is_point(up_rt) ||
            up_rt == '^' || up_rt == '_' ||

            is_solid_dline(dn_lf) || is_bot_vertex(dn_lf) || is_point(dn_lf) ||
            dn_lf == 'v' || dn_lf == '_'
        }
        else if c == '.' || c == '^' {
            dn_lf == '/'
        }
        else if c == '\'' || c == 'v' {
            up_rt == '/'
        }
        else if is_vertex(c) || is_point(c) || c == '|' {
            is_solid_dline(dn_lf) || is_solid_dline(up_rt)
        }
        else {
            false
        }
    }



}

#[inline(always)]
fn check<T, F: Fn(T) -> bool>(f: F, t: Option<T>) -> bool {
    match t {
        Some(a) => f(a),
        None => false,
    }
}

impl<'a> From<&'a str> for Grid {
    fn from(s: &str) -> Grid {
        let mut final_width = MonoWidth(0);
        let mut height = 0;
        let mut orig_widths = vec![];
        for line in s.lines() {
            let cur_width = MonoWidth(UnicodeWidthStr::width(line));
            orig_widths.push(cur_width);
            final_width = max(final_width, cur_width);
            height += 1;
        }
        let mut data: Vec<String> = Vec::with_capacity(height);
        let MonoWidth(final_width_raw) = final_width;
        for (i, (line, MonoWidth(len))) in s.lines().zip(&orig_widths).enumerate() {
            data[i] = line.clone().to_string();
            (0..(final_width_raw - len)).map(|_| data[i].push(' '));
        }
        Grid {
            width: final_width,
            height,
            data,
        }
    }
}
