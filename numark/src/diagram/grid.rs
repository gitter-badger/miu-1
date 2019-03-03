// TODO: Everything is marked as pub for now but that should be changed later.
// NOTE: Comments marked with [MM] have been directly copied from Markdeep's source
// code (written by Morgan McGuire).

use super::decoration::*;
use super::path::*;
use super::primitives::*;
use super::v2::*;

use fnv::FnvHashMap;
use unicode_width::UnicodeWidthChar;
use unicode_width::UnicodeWidthStr;

use std::cell::RefCell;
use std::cmp::max;
use std::convert::TryFrom;
use std::convert::TryInto;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Newtype wrapper for denoting character widths in integer multiples.
pub struct MonoWidth(usize);

impl MonoWidth {
    fn get(&self) -> usize {
        let MonoWidth(w) = self;
        *w
    }
}

type PosCache = FnvHashMap<(usize, usize), (CharMatch, char)>;

/// A Grid is an "unparsed" diagram -- it has characters but doesn't recognize
/// shapes in the diagram by itself.
#[derive(Debug)]
pub struct Grid {
    /// Width according to a monospace font. This may or may not match the
    /// length of any String in data.
    width: MonoWidth,

    height: usize,

    // TODO: Explain why we're using String here instead of [char] or
    // something else
    data: Vec<String>,

    /// Avoid going through data every time to get a character at a certain
    /// position.
    /// NOTE: Proper operation relies on not providing a way to mutate `data`.
    pos_cache: RefCell<PosCache>,

    // Yes, I recognize this is wasteful but we don't really expect
    // people to make humongous diagrams, so it should be okay...
    used: Vec<bool>,
}

/// In monospace fonts, Unicode assigns characters widths of 0, 1 or 2.
//
//     http://www.unicode.org/reports/tr11/
//
/// (For now, we ignore complications due to locale...)
/// So when we are asked for "the character" (1-M wide) at a position (x, y),
/// the location may correspond to either the left or right of a 2-M wide char
/// or to a 1-M wide char.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
// Skip formatting because rustfmt messes up the conditionals.
#[cfg_attr(rustfmt, rustfmt_skip)]
impl Grid {

    pub fn width(&self) -> V2EltBase {
        self.width.get().try_into().unwrap()
    }

    pub fn height(&self) -> V2EltBase {
        self.height.try_into().unwrap()
    }

    /// Basically `at_precise` but only considering `Exact` values.
    pub fn at<T: IsV2>(&self, v: T) -> Option<char> {
        match self.at_precise(v) {
            (CharMatch::Exact, c) => Some(c),
            _ => None,
        }
    }

    /// Basically `at_precise` but replacing 2-M wide chars with a
    /// Unicode surrogate characters for easy comparison failure
    /// with non-surrogate characters (which are legal in UTF-8).
    ///
    /// Use with care.
    pub fn at_faux<T: IsV2>(&self, v: T) -> char {
        match self.at_precise(v) {
            (CharMatch::Exact, c) => c,
            _ => FAUX_CHAR,
        }
    }

    /// The returned value is None whenever the corresponding "character" has
    /// width > 1. For example "ありがとう" will return `None` for
    /// `0 <= x < 10` and `y == 0`.
    /// Note that in the presence of zero-width characters, it may happen that
    /// the returned char doesn't fully incorporate what will be displayed.
    pub fn at_precise<T: IsV2>(&self, v: T) -> (CharMatch, char) {
        let v = v.to_v2();
        let x = v.x.round();
        let y = v.y.round();
        let mut cache = self.pos_cache.borrow_mut();
        cache.entry((x, y))
            .or_insert_with(|| {
                // Markdeep uses this same "hack" -- immersing the diagram in a
                // sea of infinite spaces.
                if ! self.in_bounds(v) {
                    return (CharMatch::Exact, ' ');
                }
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
            }).clone()
    }

    #[inline]
    pub fn in_bounds(&self, v: V2) -> bool {
        v.y.round() < self.height && MonoWidth(v.x.round()) < self.width
    }

    pub fn is_used<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();
        self.in_bounds(v)
            && self.used[v.y.round() * (self.width.get() + 1) + v.x.round()]
    }

    pub fn set_used<T: IsV2>(&mut self, vv: T) {
        let v = vv.to_v2();
        if self.in_bounds(v) {
            self.used[v.y.round() * (self.width.get() + 1) + v.x.round()] = true;
        }
    }

    pub fn is_solid_vline_at<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();

        let up_lf = self.at_faux(v.up().lf());
        let up    = self.at_faux(v.up());
        let up_rt = self.at_faux(v.up().rt());
        let c     = self.at_faux(v);
        let dn_lf = self.at_faux(v.dn().lf());
        let dn    = self.at_faux(v.dn());
        let dn_rt = self.at_faux(v.dn().rt());
        if is_solid_vline(c) {
            // Looks like a vertical line...does it continue?
            is_top_vertex(up) || up == '^' || is_solid_vline(up) || is_jump(up) ||
            is_bot_vertex(dn) || dn == 'v' || is_solid_vline(dn) || is_jump(dn) ||

            // TODO: Think whether dn_rt and dn_lf should be checked here.
            is_point(up) || is_point(dn) || up == '_' ||
            up_lf == '_' || up_rt == '_' ||

            // Special case of 1-high vertical on two curved corners
            ((is_top_vertex(up_lf) || is_top_vertex(up_rt))
             && (is_bot_vertex(dn_lf) || is_bot_vertex(dn_rt)))
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

    pub fn is_solid_hline_at<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();

        let lf_lf = self.at_faux(v.lf_n(2));
        let lf    = self.at_faux(v.lf());
        let c     = self.at_faux(v);
        let rt    = self.at_faux(v.rt());
        let rt_rt = self.at_faux(v.rt_n(2));

        if is_solid_hline(c) || (is_solid_hline(lf) && is_jump(c)) {
            // [MM] Looks like a horizontal line...does it continue? We need three in a row.
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

    pub fn is_solid_bline_at<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();

        let up_lf = self.at_faux(v.up().lf());
        let up    = self.at_faux(v.lf());
        let c     = self.at_faux(v);
        let dn    = self.at_faux(v.rt());
        let dn_rt = self.at_faux(v.dn().rt());

        if c == '\\' {
            // [MM] Looks like a diagonal line...does it continue? We need two in a row.
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
        else if c == 'v' {
            up_lf == '\\'
        }
        else if is_vertex(c) || is_point(c) || c == '|' {
            is_solid_bline(up_lf) || is_solid_bline(dn_rt)
        }
        else {
            false
        }
    }

    pub fn is_solid_dline_at<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();

        let up    = self.at_faux(v.up());
        let up_rt = self.at_faux(v.up().rt());
        let c     = self.at_faux(v);
        let dn_lf = self.at_faux(v.dn().lf());
        let dn    = self.at_faux(v.dn());

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

#[cfg_attr(rustfmt, rustfmt_skip)]
impl<'a> From<&'a str> for Grid {
    fn from(s: &str) -> Grid {
        let mut width = MonoWidth(0);
        let mut height = 0;
        let mut orig_widths = vec![];
        for line in s.lines() {
            let cur_width = MonoWidth(UnicodeWidthStr::width(line));
            orig_widths.push(cur_width);
            width = max(width, cur_width);
            height += 1;
        }
        let final_width = width.get();
        let mut data: Vec<String> = Vec::with_capacity(height);
        for (i, (line, MonoWidth(len))) in s.lines().zip(&orig_widths).enumerate() {
            println!("i: {}, line: {}, len: {}", i, line, len);
            // We've only set the capacity, the length still needs to be incremented.
            data.push(line.clone().to_string());
            // data[i] = line.clone().to_string();
            for _ in 0..(final_width - len) {
                data[i].push(' ');
            }
        }
        let used = vec![false; height * final_width];
        Grid {width, height, data, pos_cache: RefCell::new(PosCache::default()), used}
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
// Does the line from a to b contain at least one c?
fn line_contains(g: &Grid, a: V2, b: V2, c: char) -> bool {
    let d = b - a.into();
    let dx = d.x;
    let dy = d.y;
    // let (dx, dy) = (D2Elt::signum(b.x as D2Elt - a.x as D2Elt), i64::signum(b.y as i64 - a.y as i64));
    let (mut x, mut y) = (a.x.into(), a.y.into());
    while x != b.x.into() || y != b.y.into() {
        if g.at(D2{x, y}.force_into()) == Some(c) {
            return true;
        }
        x += dx;
        y += dy;
    }
    // Last point
    g.at(D2{x, y}.force_into()) == Some(c)
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_solid_vlines(g: &mut Grid, ps: &mut PathSet) {
    // [MM] Find all solid vertical lines. Iterate horizontally
    // so that we never hit the same line twice
    for x in 0..g.width() {
        let mut y = 0;
        while y < g.height() {
            let cur = (x, y).to_v2();
            // TODO: Refactor this block out into a function.
            if g.is_solid_vline_at(cur) {
                // [MM] This character begins a vertical line...now, find the end
                let mut a = cur.clone();
                // Replacement for do-while loop
                g.set_used((x, y));
                y += 1;
                while g.is_solid_vline_at((x, y)) {
                    g.set_used((x, y));
                    y += 1;
                }
                let mut b = (x, y - 1).to_v2();

                {
                    let up = g.at_faux(a);
                    let up_up = g.at_faux(a.up());

                    // TODO: Understand black magic.
                    if !is_vertex(up)
                        && (up_up == '-'
                            || up_up == '_'
                            || is_bot_vertex(up_up)
                            || is_jump(up_up)
                            || g.at_faux(a.up().lf()) == '_'
                            || g.at_faux(a.up().rt()) == '_')
                    {
                        // [MM] Stretch up to almost reach the line above
                        // (if there is a decoration, it will finish the gap)
                        a.y -= Offset::HALF;
                    }
                }

                {
                    let dn = g.at_faux(b);
                    let dn_dn = g.at_faux(b.dn());

                    // TODO: Understand black magic.
                    if !is_vertex(dn)
                        && (dn_dn == '-'
                            || is_top_vertex(dn_dn)
                            || is_jump(dn_dn)
                            || g.at_faux(b.lf()) == '_'
                            || g.at_faux(b.rt()) == '_')
                    {
                        // [MM] Stretch down to almost reach the line below
                        b.y += Offset::HALF;
                    }
                }

                // [MM] Don't insert degenerate lines
                if a != b {
                    ps.insert(Path::straight(a, b));
                }

            // [MM] Continue the search from the end value y+1
            }
            // [MM] Some very special patterns for the short lines needed on
            // circuit diagrams. Only invoke these if not also on a curve
            //      _  _
            //    -'    '-
            else if g.at_faux(cur) == '\''
                && (   (g.at_faux(cur.lf()) == '-'
                        && g.at_faux(cur.up().rt()) == '_'
                        && !is_solid_vline_or_jump_or_point(g.at_faux(cur.up().lf())))
                    || (g.at_faux(cur.up().lf()) == '_'
                        && g.at_faux(cur.rt()) == '-'
                        && !is_solid_vline_or_jump_or_point(g.at_faux(cur.up().rt()))))
            {
                ps.insert(Path::straight(
                    (cur.x, cur.y - Offset::HALF).to_v2(),
                    cur.clone(),
                ));
            }
            // [MM]   _.-  -._
            else if g.at_faux(cur) == '.'
                && (   (g.at_faux(cur.lf()) == '_'
                        && g.at_faux(cur.rt()) == '-'
                        && !is_solid_vline_or_jump_or_point(g.at_faux(cur.dn().rt())))
                    || (g.at_faux(cur.lf()) == '-'
                        && g.at_faux(cur.rt()) == '_'
                        && !is_solid_vline_or_jump_or_point(g.at_faux(cur.dn().lf()))))
            {
                ps.insert(Path::straight(
                    cur.clone(),
                    (cur.x, cur.y + Offset::HALF).to_v2(),
                ));
            }
            y += 1;
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_solid_hlines(g: &mut Grid, ps: &mut PathSet) {
    let mut y = 0;
    while y < g.height() {
        let mut x = 0;
        while x < g.width() {
            let cur = (x, y).to_v2();
            if g.is_solid_hline_at(cur) {
                // [MM] Begins a line...find the end
                let mut a = cur.clone();
                // Replacement for do-while loop
                g.set_used((x, y));
                x += 1;
                while g.is_solid_hline_at((x, y)) {
                    g.set_used((x, y));
                    x += 1;
                }
                let mut b = (x - 1, y).to_v2();

                // [MM] Detect curves and shorten the edge
                if !is_vertex(g.at_faux(a.lf()))
                    && (   (is_top_vertex(g.at_faux(a))
                            && is_solid_vline_or_jump_or_point(g.at_faux(a.dn().lf())))
                        || (is_bot_vertex(g.at_faux(a))
                            && is_solid_vline_or_jump_or_point(g.at_faux(a.up().lf()))))
                {
                    a = a.rt();
                }

                if !is_vertex(g.at_faux(b.rt()))
                    && (   (is_top_vertex(g.at_faux(b))
                            && is_solid_vline_or_jump_or_point(g.at_faux(b.dn().rt())))
                        || (is_bot_vertex(g.at_faux(b))
                            && is_solid_vline_or_jump_or_point(g.at_faux(b.up().rt()))))
                {
                    b = b.lf();
                }

                // [MM] Don't insert degenerate lines
                if a != b {
                    ps.insert(Path::straight(a, b));
                }
                // [MM] Continue the search from the end x+1
            }
            x += 1;
        }
        y += 1;
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_solid_blines(g: &mut Grid, ps: &mut PathSet) {
    for i in -(g.width() as D2EltBase) .. (g.height() as D2EltBase) {
        // Yay type safety!
        let iters = (i ..).zip(0 .. g.height())
            .filter_map(|(x, y)| V2EltBase::try_from(x).ok().map(|x| (x, y)));
        for (mut x, mut y) in iters {
            let cur = (x, y).to_v2();
            if g.is_solid_bline_at(cur) {
                // [MM] Begins a line...find the end
                let mut a = cur.clone();
                // Desugar do-while loop
                x += 1;
                y += 1;
                while g.is_solid_bline_at((x, y)) {
                    x += 1;
                    y += 1;
                }
                let mut b = (x - 1, y - 1).to_v2();

                // Ensure that the entire line wasn't just vertices
                if line_contains(g, a, b, '\\') {
                    assert!(a.is_exact() && b.is_exact());
                    for j in a.x.round() ..= b.x.round() {
                        // Why the fuck is there no TryFrom<usize> for u32?
                        let conv = |x: usize| -> V2Elt { x.try_into().unwrap() };
                        g.set_used((conv(j), conv(a.y.round() + (j - a.x.round()))));
                    }
                    {
                        // Top point of the backdiag
                        let up_lf = g.at_faux(a.up().lf());
                        let up = g.at_faux(a.up());
                        let bd_top = g.at_faux(a);
                        if up == '/' || up_lf == '_' || up == '_'
                            || (!is_vertex(bd_top)
                                && (is_solid_hline(up_lf) || is_solid_vline(up_lf))) {
                            // Continue half a cell more to connect for:
                            //  ___   ___
                            //  \        \    /      ----     |
                            //   \        \   \        ^      |^
                            a.x -= Offset::HALF;
                            a.y -= Offset::HALF;
                        } else if is_point(up_lf) {
                            // Continue 1/4 cell more to connect for:
                            //
                            //  o
                            //   ^
                            //    \
                            a.x -= Offset::QUARTER;
                            a.y -= Offset::QUARTER;
                        }
                    }
                    {
                        // Bottom point of the backdiag
                        let lf = g.at_faux(b.lf());
                        let bd_bot = g.at_faux(b);
                        let rt = g.at_faux(b.rt());
                        let dn = g.at_faux(b.dn());
                        let dn_rt = g.at_faux(b.dn().rt());
                        if dn == '/' || rt == '_' || lf == '_'
                            || (!is_vertex(bd_bot)
                                && (is_solid_hline(dn_rt) || is_solid_vline(dn_rt))) {
                            // Continue half a cell more to connect for:
                            //                       \      \ |
                            //  \       \     \       v      v|
                            //   \__   __\    /      ----     |
                            b.x += Offset::HALF;
                            b.y += Offset::HALF;
                        } else if is_point(dn_rt) {
                            // Continue 1/4 cell more to connect for:
                            //
                            //    \
                            //     v
                            //      o
                            b.x += Offset::QUARTER;
                            b.y += Offset::QUARTER;
                        }
                    }
                    ps.insert(Path::straight(a, b));
                    // [MM] Continue the search from the end x+1,y+1
                }
            }
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_solid_dlines(g: &mut Grid, ps: &mut PathSet) {
    // [MM] Find all solid left-to-right upward diagonal lines (DIAGONAL)
    for i in -(g.height() as D2EltBase) .. (g.width() as D2EltBase) {
        // Yay type safety!
        let iters = (i ..).zip((g.height() - 1) ..= 0)
            .filter_map(|(x, y)| V2EltBase::try_from(x).ok().map(|x| (x, y)));
        for (mut x, mut y) in iters {
            let cur = (x, y).to_v2();
            if g.is_solid_dline_at(cur) {
                // [MM] Begins a line...find the end
                let mut a = cur.clone();
                // Desugar do-while loop
                x += 1;
                y -= 1;
                while g.is_solid_dline_at((x, y)) {
                    x += 1;
                    y -= 1;
                }
                let mut b = (x - 1, y + 1).to_v2();

                if line_contains(g, a, b, '/') {
                    assert!(a.is_exact() && b.is_exact());
                    // [MM] This is definitely a line. Commit the characters on it
                    for j in a.x.round() ..= b.x.round() {
                        // Why the fuck is there no TryFrom<usize> for u32?
                        let conv = |x: usize| -> V2Elt { x.try_into().unwrap() };
                        g.set_used((conv(j), conv(a.y.round() - (j - a.x.round()))));
                    }
                    {
                        // Bottom point for diagonal
                        let up = g.at_faux(b.up());
                        let up_rt = g.at_faux(b.up().rt());
                        let dg_bot = g.at_faux(b);

                        if up == '\\' || up == '_' || up_rt == '_'
                            || (! is_vertex(dg_bot)
                                && (is_solid_hline(up_rt) || is_solid_vline(up_rt))) {
                            // [MM] Continue half a cell more to connect at:
                            //     __   __  ---     |
                            //    /      /   ^     ^|
                            //   /      /   /     / |
                            b.x += Offset::HALF;
                            b.y -= Offset::HALF;
                        } else if is_point(up_rt) {
                            // [MM] Continue 1/4 cell more to connect at:
                            //
                            //       o
                            //      ^
                            //     /
                            b.x += Offset::QUARTER;
                            b.y -= Offset::QUARTER;
                        }
                    }
                    {
                        let lf = g.at_faux(a.lf());
                        let dg_top = g.at_faux(a);
                        let rt = g.at_faux(a.rt());
                        let dn_lf = g.at_faux(a.dn().lf());
                        let dn = g.at_faux(a.dn());
                        if dn == '\\' || lf == '_' || rt == '_'
                            || (!is_vertex(dg_top)
                                && (is_solid_hline(dn_lf) || is_solid_vline(dn_lf))) {
                            // Continue half a cell more to connect at:
                            //               /     \ |
                            //    /  /      v       v|
                            // __/  /__   ----       |
                            a.x -= Offset::HALF;
                            a.y += Offset::HALF;
                        } else if is_point(dn_lf) {
                            // Continue 1/4 cell more to connect at:
                            //
                            //       /
                            //      v
                            //     o
                            a.x -= Offset::QUARTER;
                            a.y += Offset::QUARTER;
                        }
                    }
                    ps.insert(Path::straight(a, b));
                    // [MM] Continue the search from the end x+1,y-1
                }
            }
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_curved_corners(g: &mut Grid, ps: &mut PathSet) {
    // [MM] Now look for curved corners. The syntax constraints require
    // that these can always be identified by looking at three
    // horizontally-adjacent characters.
    for y in 0 .. g.height() {
        for x in 0 .. g.width() {
            let v = (x, y).to_v2();

            let up_lf = g.at_faux(v.up().lf());
            let up_rt = g.at_faux(v.up().rt());
            let lf    = g.at_faux(v.lf());
            let c     = g.at_faux(v);
            let rt    = g.at_faux(v.rt());
            let dn_lf = g.at_faux(v.dn().lf());
            let dn_rt = g.at_faux(v.dn().rt());

            // [MM] Note that because of undirected vertices, the
            // following cases are not exclusive
            if is_top_vertex(c) {
                // [MM] -.
                //        |
                if is_solid_hline(lf) && is_solid_vline(dn_rt) {
                    g.set_used(v.lf());
                    g.set_used(v);
                    g.set_used(v.dn().rt());
                    let c_pt = (x.to_v2elt() + Offset::ratio(1, 10), y.to_v2elt()).to_v2();
                    let d_pt = v.dn().rt();
                    ps.insert(Path::curved(v.lf(), v.dn().rt(), c_pt, d_pt));
                }

                // [MM]  .-
                //      |
                if is_solid_hline(rt) && is_solid_vline(dn_lf) {
                    g.set_used(v.dn().lf());
                    g.set_used(v);
                    g.set_used(v.rt());
                    let c_pt = (x.to_v2elt() - Offset::ratio(1, 10), y.to_v2elt()).to_v2();
                    let d_pt = v.dn().lf();
                    ps.insert(Path::curved(v.rt(), v.dn().lf(), c_pt, d_pt));
                }
            }

            // [MM] Special case patterns:
            //   .  .   .  .
            //  (  o     )  o
            //   '  .   '  '
            if (c == ')' || is_point(c)) && up_lf == '.' && dn_lf == '\'' {
                g.set_used(v);
                g.set_used(v.up().lf());
                g.set_used(v.dn().lf());
                let c_pt = (x.to_v2elt() + Offset::ratio(6, 10), (y - 1).to_v2elt()).to_v2();
                let d_pt = (x.to_v2elt() + Offset::ratio(6, 10), (y + 1).to_v2elt()).to_v2();
                ps.insert(Path::curved(v.up().lf_n(2), v.dn().lf_n(2), c_pt, d_pt));
            }

            if (c == '(' || is_point(c)) && up_rt == '.' && dn_rt == '\'' {
                g.set_used(v);
                g.set_used(v.up().rt());
                g.set_used(v.dn().rt());
                let c_pt = (x.to_v2elt() - Offset::ratio(6, 10), (y - 1).to_v2elt()).to_v2();
                let d_pt = (x.to_v2elt() - Offset::ratio(6, 10), (y + 1).to_v2elt()).to_v2();
                ps.insert(Path::curved(v.up().rt_n(2), v.dn().rt_n(2), c_pt, d_pt));
            }

            if is_bot_vertex(c) {
                // [MM]   |
                //      -'
                if is_solid_hline(lf) && is_solid_vline(up_rt) {
                    g.set_used(v.lf());
                    g.set_used(v);
                    g.set_used(v.up().rt());
                    let c_pt = (x.to_v2elt() + Offset::ratio(1, 10), y.to_v2elt()).to_v2();
                    let d_pt = v.up().rt();
                    ps.insert(Path::curved(v.lf(), v.up().rt(), c_pt, d_pt));
                }

                // [MM] |
                //       '-
                if is_solid_hline(rt) && is_solid_vline(up_lf) {
                    g.set_used(v.up().lf());
                    g.set_used(v);
                    g.set_used(v.rt());
                    let c_pt = (x.to_v2elt() - Offset::ratio(1, 10), y.to_v2elt()).to_v2();
                    let d_pt = v.up().lf();
                    ps.insert(Path::curved(v.rt(), v.up().lf(), c_pt, d_pt));
                }
            }
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn find_low_horizontal_lines(g: &mut Grid, ps: &mut PathSet) {
    // [MM] Find low horizontal lines marked with underscores. These
    // are so simple compared to the other cases that we process
    // them directly here without a helper function. Process these
    // from top to bottom and left to right so that we can read
    // them in a single sweep.
    //
    // Exclude the special case of double underscores going right
    // into an ASCII character, which could be a source code
    // identifier such as __FILE__ embedded in the diagram.
    for y in 0 .. g.height() {
        let mut x = 2;
        while x < g.width() - 2 {
            let v = (x, y).to_v2();

            let up_lf = g.at_faux(v.up().lf());
            let up    = g.at_faux(v.up());
            let lf_lf = g.at_faux(v.lf_n(2));
            let lf    = g.at_faux(v.lf());
            let c     = g.at_faux(v);
            let rt    = g.at_faux(v.rt());
            let rt_rt = g.at_faux(v.rt_n(2));
            let dn_lf = g.at_faux(v.dn().lf());
            let dn    = g.at_faux(v.dn());

            if c == '_' && rt == '_'
                && (!rt_rt.is_ascii() || lf == '_')
                && (!lf.is_ascii() || rt_rt == '_') {

                let mut a = (x.to_v2elt() - Offset::HALF, y.to_v2elt() + Offset::HALF).to_v2();

                if lf == '|' || dn_lf == '|' || lf == '.' || dn_lf == '\'' {
                    // [MM] Extend to meet adjacent vertical
                    a.x -= Offset::HALF;

                    // [MM] Very special case of overrunning into the side of a curve,
                    // needed for logic gate diagrams
                    if lf == '.'
                        && (lf_lf == '-' || lf_lf == '.')
                        && g.at_faux(v.dn().lf_n(2)) == '(' {
                        a.x -= Offset::HALF;
                    }
                } else if lf == '/' {
                    a.x = (a.x - 1.to_v2elt()).try_into().unwrap();
                }

                // Detect overrun of a tight double curve
                if lf == '(' && lf_lf == '(' && dn == '\'' && up == '.' {
                    a.x += Offset::HALF;
                }
                // lf = lf_lf = undefined;

                // Desugared do-while loop
                g.set_used((x, y));
                x += 1;
                while g.at_faux((x, y)) == '_' {
                    g.set_used((x, y));
                    x += 1;
                }

                let v = (x, y).to_v2();
                let mut b = (v.x - Offset::HALF, v.y + Offset::HALF).to_v2();
                let rt = g.at_faux(v.rt());
                let c = g.at_faux(v);
                let dn = g.at_faux(v.dn());
                let dn_rt = g.at_faux(v.dn().rt());

                if c == '|' || dn == '|' || c == '.' || dn == '\'' {
                    // [MM] Extend to meet adjacent vertical
                    b.x += Offset::HALF;

                    // [MM] Very special case of overrunning into the side of a curve,
                    // needed for logic gate diagrams
                    if c == '.' && (rt == '-' || rt == '.') && dn_rt == ')' {
                        b.x += Offset::HALF;
                    }
                } else if c == '\\' {
                    b.x = (b.x + 1.to_v2elt()).try_into().unwrap();
                }

                // [MM] Detect overrun of a tight double curve
                if c == ')' && rt == ')' && dn_lf == '\'' && up_lf == '.' {
                    b.x -= Offset::HALF;
                }
                ps.insert(Path::straight(a, b));
            }
            x += 1;
        }
    }
}

pub fn find_paths(g: &mut Grid, ps: &mut PathSet) {
    find_solid_vlines(g, ps);
    find_solid_hlines(g, ps);
    find_solid_blines(g, ps);
    find_solid_dlines(g, ps);
    find_curved_corners(g, ps);
    find_low_horizontal_lines(g, ps);
}

#[cfg_attr(rustfmt, rustfmt_skip)]
pub fn find_decorations(g: &mut Grid, ps: &mut PathSet, decors: &mut DecorationSet) {
    // The name used in Markdeep for this function "isEmptyOrVertex" doesn't
    // seem appropriate because there is already a separate is_vertex function
    // which is not being used here.
    let is_space_or_decor = |c: char| {
        c == ' ' || c == 'o' || c == 'v' || !c.is_alphanumeric()
    };

    // [MM] Is the point in the center of these values on a line? Allow points
    // that are vertically adjacent but not horizontally--they wouldn't fit
    // anyway, and might be text.
    let on_line = |up: char, dn: char, lf: char, rt: char| {
           (is_space_or_decor(dn) || is_point(dn))
        && (is_space_or_decor(up) || is_point(up))
        && is_space_or_decor(rt)
        && is_space_or_decor(lf)
    };

    for x in 0 .. g.width() {
        for j in 0 .. g.height() {
            let c = g.at_faux((x, j));
            let y = j;
            let v = (x, y).to_v2();

            if is_jump(c) {
                // [MM] Ensure that this is really a jump and not a stray character
                if ps.dn_ends_at((v.x, v.y - Offset::HALF))
                    && ps.up_ends_at((v.x, v.y + Offset::HALF)) {
                    decors.insert((v, c));
                    g.set_used(v);
                }
            }
            else if is_point(c) {
                let up = g.at_faux(v.up());
                let dn = g.at_faux(v.dn());
                let lf = g.at_faux(v.lf());
                let rt = g.at_faux(v.rt());

                if ps.rt_ends_at(v.lf())     // [MM] Must be at the end of a line...
                    || ps.lf_ends_at(v.rt()) // [MM] or completely isolated NSEW
                    || ps.dn_ends_at(v.up())
                    || ps.up_ends_at(v.dn())
                    || ps.up_ends_at(v)      // [MM] For points on vertical lines
                    || ps.dn_ends_at(v)      // [MM] that are surrounded by other characters
                    || on_line(up, dn, lf, rt) {
                    decors.insert((v, c));
                    g.set_used(v);
                }
                else if is_gray(c) {
                    decors.insert((v, c));
                    g.set_used(v);
                }
                else if is_tri(c) {
                    decors.insert((v, c));
                    g.set_used(v);
                } else {
                    find_arrowheads(g, ps, decors, v, c);
                }
            } // y
        } // x
    }
}

fn find_arrowheads(g: &mut Grid, ps: &mut PathSet, decors: &mut DecorationSet, v: V2, c: char) {
    let lf = v.lf();
    let rt = v.rt();

    let mut is_used = true;

    // [MM] If we find one, ensure that it is really an
    // arrow head and not a stray character by looking
    // for a connecting line.
    if c == '>' && (ps.rt_ends_at(v) || ps.horizontal_passes_thru(v)) {
        let mut dx = Offset::ZERO;
        if is_point(g.at_faux(rt)) {
            // [MM] Back up if connecting to a point so as to
            // not overlap it
            dx = Offset::HALF;
        }
        decors.insert((v.x - dx, v.y, '>', Angle::A0));
    } else if c == '<' && (ps.lf_ends_at(v) || ps.horizontal_passes_thru(v)) {
        let mut dx = Offset::ZERO;
        if is_point(g.at_faux(lf)) {
            // [MM] Back up if connecting to a point so as to
            // not overlap it
            dx = Offset::HALF;
        }
        decors.insert((v.x + dx, v.y, '>', Angle::A180));
    } else if c == '^' {
        // [MM] Because of the aspect ratio, we need to look
        // in two slots for the end of the previous line
        // TODO: Fix indentation here.
        if ps.up_ends_at((v.x, v.y - Offset::HALF)) {
            decors.insert((v.x, v.y - Offset::HALF, '>', Angle::A270));
        } else if ps.up_ends_at(v) {
            decors.insert((v, '>', Angle::A270));
        } else if ps.diagonal_up_ends_at((v.x + Offset::HALF, v.y - Offset::HALF)) {
            decors.insert((
                v.x + Offset::HALF,
                v.y - Offset::HALF,
                '>',
                Angle::A270 + Angle::DIAGONAL,
            ));
        } else if ps.diagonal_up_ends_at((v.x + Offset::QUARTER, v.y - Offset::QUARTER)) {
            decors.insert((
                v.x + Offset::QUARTER,
                v.y - Offset::QUARTER,
                '>',
                Angle::A270 + Angle::DIAGONAL,
            ));
        } else if ps.diagonal_up_ends_at((v.x, v.y)) {
            decors.insert((v.x, v.y, '>', Angle::A270 + Angle::DIAGONAL));
        } else if ps.backdiag_up_ends_at((v.x, v.y)) {
            decors.insert((v.x, v.y, c, Angle::A270 - Angle::DIAGONAL));
        } else if ps.backdiag_up_ends_at((v.x - Offset::HALF, v.y - Offset::HALF)) {
            decors.insert((
                v.x - Offset::HALF,
                v.y - Offset::HALF,
                c,
                Angle::A270 - Angle::DIAGONAL,
            ));
        } else if ps.backdiag_up_ends_at((v.x - Offset::QUARTER, v.y - Offset::QUARTER)) {
            decors.insert((
                v.x - Offset::QUARTER,
                v.y - Offset::QUARTER,
                c,
                Angle::A270 - Angle::DIAGONAL,
            ));
        } else if ps.vertical_passes_thru((v.x, v.y)) {
            // Only try this if all others failed
            decors.insert((v.x, v.y - Offset::HALF, '>', Angle::A270));
        } else {
            is_used = false;
        }
    } else if c == 'v' {
        if ps.dn_ends_at((v.x, v.y + Offset::HALF)) {
            decors.insert((v.x, v.y + Offset::HALF, '>', Angle::A90));
        } else if ps.dn_ends_at(v) {
            decors.insert((v.x, v.y, '>', Angle::A90));
        } else if ps.diagonal_dn_ends_at(v) {
            decors.insert((v.x, v.y, '>', Angle::A90 + Angle::DIAGONAL));
        } else if ps.diagonal_dn_ends_at((v.x - Offset::HALF, v.y + Offset::HALF)) {
            decors.insert((
                v.x - Offset::HALF,
                v.y + Offset::HALF,
                '>',
                Angle::A90 + Angle::DIAGONAL,
            ));
        } else if ps.diagonal_dn_ends_at((v.x - Offset::QUARTER, v.y + Offset::QUARTER)) {
            decors.insert((
                v.x - Offset::QUARTER,
                v.y + Offset::QUARTER,
                '>',
                Angle::A90 + Angle::DIAGONAL,
            ));
        } else if ps.backdiag_dn_ends_at(v) {
            decors.insert((v.x, v.y, '>', Angle::A90 - Angle::DIAGONAL));
        } else if ps.backdiag_dn_ends_at((v.x + Offset::HALF, v.y + Offset::HALF)) {
            decors.insert((
                v.x + Offset::HALF,
                v.y + Offset::HALF,
                '>',
                Angle::A90 - Angle::DIAGONAL,
            ));
        } else if ps.backdiag_dn_ends_at((v.x + Offset::QUARTER, v.y + Offset::QUARTER)) {
            decors.insert((
                v.x + Offset::QUARTER,
                v.y + Offset::QUARTER,
                '>',
                Angle::A90 - Angle::DIAGONAL,
            ));
        } else if ps.vertical_passes_thru(v) {
            // [MM] Only try this if all others failed
            decors.insert((v.x, v.y + Offset::HALF, '>', Angle::A90));
        } else {
            is_used = false;
        }
    } else {
        is_used = false;
    }

    if is_used {
        g.set_used(v);
    }
}
