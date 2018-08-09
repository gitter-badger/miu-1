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

impl Grid {
    pub fn at(&self, x: usize, y: usize) -> Option<char> {
        match self.at_precise(x, y) {
            (Exact, c) => Some(c),
            _ => None,
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
                1 if (x == tmp) => { return (CharMatch::Exact, c); },
                2 if (x == tmp) => { return (CharMatch::Left, c); },
                2 if (x == tmp + 1) => { return (CharMatch::Right, c); },
                // 0 => (),
                _ => { tmp += sz; }
            }
        }
        unreachable!();
    }

    pub fn is_solid_vline_at(&self, x: usize, y: usize) -> bool {
        let up = self.at(x, y - 1);
        let c = self.at(x, y);
        let dn = self.at(x, y + 1);
        let up_rt = self.at(x + 1, y - 1);
        let up_lf = self.at(x - 1, y - 1);
        if (check(is_solid_vline, c)) {
            false
        } else {
            false
        }
    }
}

#[inline(always)]
fn check<T, F:Fn(T) -> bool>(f: F, t: Option<T>) -> bool {
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
            (0 .. (final_width_raw - len)).map(|_| data[i].push(' '));
        }
        Grid { width: final_width, height, data }
    }
}
