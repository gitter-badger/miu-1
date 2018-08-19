// TODO: Everything is marked as pub for now but that should be changed later.

use diagram::path::*;
use diagram::primitives::*;
use diagram::v2::*;

use fnv::FnvHashMap;
use unicode_width::UnicodeWidthChar;
use unicode_width::UnicodeWidthStr;

use std::cell::RefCell;
use std::cmp::max;
use std::convert::TryInto;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Newtype wrapper for denoting character widths in integer multiples.
pub struct MonoWidth(usize);

impl MonoWidth {
    fn unwrap(&self) -> usize {
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
///
///     http://www.unicode.org/reports/tr11/
///
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
#[cfg_attr(rustfmt, rustfmt_skip)]
impl Grid {

    pub fn width(&self) -> V2EltBase {
        self.width.unwrap().try_into().unwrap()
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
        let vv = v.to_v2();
        let x = vv.x.round();
        let y = vv.y.round();
        let mut cache = self.pos_cache.borrow_mut();
        cache.entry((x, y))
            .or_insert_with(|| {
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
            }).clone()
    }

    pub fn is_used<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();
        self.used[v.y.round() * (self.width.unwrap() + 1) + v.x.round()]
    }

    pub fn set_used<T: IsV2>(&mut self, vv: T) {
        let v = vv.to_v2();
        self.used[v.y.round() * (self.width.unwrap() + 1) + v.x.round()] = true;
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

    pub fn is_solid_bline_at<T: IsV2>(&self, vv: T) -> bool {
        let v = vv.to_v2();

        let up_lf = self.at_faux(v.up().lf());
        let up    = self.at_faux(v.lf());
        let c     = self.at_faux(v);
        let dn    = self.at_faux(v.rt());
        let dn_rt = self.at_faux(v.dn().rt());

        if c == '\\' {
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
        let final_width = width.unwrap();
        let mut data: Vec<String> = Vec::with_capacity(height);
        for (i, (line, MonoWidth(len))) in s.lines().zip(&orig_widths).enumerate() {
            data[i] = line.clone().to_string();
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
fn find_paths(g: &mut Grid, ps: &mut PathSet) {
    // Find all solid vertical lines. Iterate horizontally
    // so that we never hit the same line twice
    for x in 0 .. g.width() {
        for mut y in 0 .. g.height() {
            let v = (x, y).to_v2();
            if g.is_solid_vline_at(v) {
                // This character begins a vertical line...now, find the end
                let mut a = (x, y).to_v2();
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
                        && (up_up == '-' || up_up == '_'
                            || is_bot_vertex(up_up) || is_jump(up_up)
                            || g.at_faux(a.up().lf()) == '_'
                            || g.at_faux(a.up().rt()) == '_') {
                        // Stretch up to almost reach the line above (if there is a decoration,
                        // it will finish the gap)
                        a.y -= Offset::HALF;
                        // a.y -= 0.5;
                    }
                }

                {
                    let dn = g.at_faux(b);
                    let dn_dn = g.at_faux(b.dn());

                    // TODO: Understand black magic.
                    if !is_vertex(dn)
                        && (dn_dn == '-'
                            || is_top_vertex(dn_dn) || is_jump(dn_dn)
                            || g.at_faux(b.lf()) == '_'
                            || g.at_faux(b.rt()) == '_') {
                        // Stretch down to almost reach the line below
                        b.y += Offset::HALF;
                    }
                }

                // Don't insert degenerate lines
                if a.x != b.x || a.y != b.y {
                    ps.insert(Path::new(a, b));
                }

                // Continue the search from the end value y+1
            }

            // // Some very special patterns for the short lines needed on
            // // circuit diagrams. Only invoke these if not also on a curve
            // //      _  _
            // //    -'    '-
            // else if ((grid(x, y) === "'") &&
            //     (((grid(x - 1, y) === '-') && (grid(x + 1, y - 1) === '_') &&
            //      ! isSolidVLineOrJumpOrPoint(grid(x - 1, y - 1))) ||
            //      ((grid(x - 1, y - 1) === '_') && (grid(x + 1, y) === '-') &&
            //      ! isSolidVLineOrJumpOrPoint(grid(x + 1, y - 1))))) {
            //     pathSet.insert(new Path(Vec2(x, y - 0.5), Vec2(x, y)));
            // }

            // //    _.-  -._
            // else if ((grid(x, y) === '.') &&
            //          (((grid(x - 1, y) === '_') && (grid(x + 1, y) === '-') &&
            //            ! isSolidVLineOrJumpOrPoint(grid(x + 1, y + 1))) ||
            //           ((grid(x - 1, y) === '-') && (grid(x + 1, y) === '_') &&
            //            ! isSolidVLineOrJumpOrPoint(grid(x - 1, y + 1))))) {
            //     pathSet.insert(new Path(Vec2(x, y), Vec2(x, y + 0.5)));
            // }

        } // y
    } // x

    // // Find all solid horizontal lines
    // for (var y = 0; y < grid.height; ++y) {
    //     for (var x = 0; x < grid.width; ++x) {
    //         if (grid.isSolidHLineAt(x, y)) {
    //             // Begins a line...find the end
    //             var A = Vec2(x, y);
    //             do { grid.setUsed(x, y); ++x; } while (grid.isSolidHLineAt(x, y));
    //             var B = Vec2(x - 1, y);

    //             // Detect curves and shorten the edge
    //             if ( ! isVertex(grid(A.x - 1, A.y)) &&
    //                  ((isTopVertex(grid(A)) && isSolidVLineOrJumpOrPoint(grid(A.x - 1, A.y + 1))) ||
    //                   (isBottomVertex(grid(A)) && isSolidVLineOrJumpOrPoint(grid(A.x - 1, A.y - 1))))) {
    //                 ++A.x;
    //             }

    //             if ( ! isVertex(grid(B.x + 1, B.y)) &&
    //                  ((isTopVertex(grid(B)) && isSolidVLineOrJumpOrPoint(grid(B.x + 1, B.y + 1))) ||
    //                   (isBottomVertex(grid(B)) && isSolidVLineOrJumpOrPoint(grid(B.x + 1, B.y - 1))))) {
    //                 --B.x;
    //             }

    //             // Don't insert degenerate lines
    //             if ((A.x !== B.x) || (A.y !== B.y)) {
    //                 pathSet.insert(new Path(A, B));
    //             }
    //             // Continue the search from the end x+1
    //         }
    //     }
    // } // y

    // // Find all solid left-to-right downward diagonal lines (BACK DIAGONAL)
    // for (var i = -grid.height; i < grid.width; ++i) {
    //     for (var x = i, y = 0; y < grid.height; ++y, ++x) {
    //         if (grid.isSolidBLineAt(x, y)) {
    //             // Begins a line...find the end
    //             var A = Vec2(x, y);
    //             do { ++x; ++y; } while (grid.isSolidBLineAt(x, y));
    //             var B = Vec2(x - 1, y - 1);

    //             // Ensure that the entire line wasn't just vertices
    //             if (lineContains(A, B, '\\')) {
    //                 for (var j = A.x; j <= B.x; ++j) {
    //                     grid.setUsed(j, A.y + (j - A.x));
    //                 }

    //                 var top = grid(A);
    //                 var up = grid(A.x, A.y - 1);
    //                 var uplt = grid(A.x - 1, A.y - 1);
    //                 if ((up === '/') || (uplt === '_') || (up === '_') ||
    //                     (! isVertex(top)  &&
    //                      (isSolidHLine(uplt) || isSolidVLine(uplt)))) {
    //                     // Continue half a cell more to connect for:
    //                     //  ___   ___
    //                     //  \        \    /      ----     |
    //                     //   \        \   \        ^      |^
    //                     A.x -= 0.5; A.y -= 0.5;
    //                 } else if (isPoint(uplt)) {
    //                     // Continue 1/4 cell more to connect for:
    //                     //
    //                     //  o
    //                     //   ^
    //                     //    \
    //                     A.x -= 0.25; A.y -= 0.25;
    //                 }

    //                 var bottom = grid(B);
    //                 var dnrt = grid(B.x + 1, B.y + 1);
    //                 if ((grid(B.x, B.y + 1) === '/') || (grid(B.x + 1, B.y) === '_') ||
    //                     (grid(B.x - 1, B.y) === '_') ||
    //                     (! isVertex(grid(B)) &&
    //                      (isSolidHLine(dnrt) || isSolidVLine(dnrt)))) {
    //                     // Continue half a cell more to connect for:
    //                     //                       \      \ |
    //                     //  \       \     \       v      v|
    //                     //   \__   __\    /      ----     |

    //                     B.x += 0.5; B.y += 0.5;
    //                 } else if (isPoint(dnrt)) {
    //                     // Continue 1/4 cell more to connect for:
    //                     //
    //                     //    \
    //                     //     v
    //                     //      o

    //                     B.x += 0.25; B.y += 0.25;
    //                 }

    //                 pathSet.insert(new Path(A, B));
    //                 // Continue the search from the end x+1,y+1
    //             } // lineContains
    //         }
    //     }
    // } // i


    // // Find all solid left-to-right upward diagonal lines (DIAGONAL)
    // for (var i = -grid.height; i < grid.width; ++i) {
    //     for (var x = i, y = grid.height - 1; y >= 0; --y, ++x) {
    //         if (grid.isSolidDLineAt(x, y)) {
    //             // Begins a line...find the end
    //             var A = Vec2(x, y);
    //             do { ++x; --y; } while (grid.isSolidDLineAt(x, y));
    //             var B = Vec2(x - 1, y + 1);

    //             if (lineContains(A, B, '/')) {
    //                 // This is definitely a line. Commit the characters on it
    //                 for (var j = A.x; j <= B.x; ++j) {
    //                     grid.setUsed(j, A.y - (j - A.x));
    //                 }

    //                 var up = grid(B.x, B.y - 1);
    //                 var uprt = grid(B.x + 1, B.y - 1);
    //                 var bottom = grid(B);
    //                 if ((up === '\\') || (up === '_') || (uprt === '_') ||
    //                     (! isVertex(grid(B)) &&
    //                      (isSolidHLine(uprt) || isSolidVLine(uprt)))) {

    //                     // Continue half a cell more to connect at:
    //                     //     __   __  ---     |
    //                     //    /      /   ^     ^|
    //                     //   /      /   /     / |

    //                     B.x += 0.5; B.y -= 0.5;
    //                 } else if (isPoint(uprt)) {

    //                     // Continue 1/4 cell more to connect at:
    //                     //
    //                     //       o
    //                     //      ^
    //                     //     /

    //                     B.x += 0.25; B.y -= 0.25;
    //                 }

    //                 var dnlt = grid(A.x - 1, A.y + 1);
    //                 var top = grid(A);
    //                 if ((grid(A.x, A.y + 1) === '\\') || (grid(A.x - 1, A.y) === '_') || (grid(A.x + 1, A.y) === '_') ||
    //                     (! isVertex(grid(A)) &&
    //                      (isSolidHLine(dnlt) || isSolidVLine(dnlt)))) {

    //                     // Continue half a cell more to connect at:
    //                     //               /     \ |
    //                     //    /  /      v       v|
    //                     // __/  /__   ----       |

    //                     A.x -= 0.5; A.y += 0.5;
    //                 } else if (isPoint(dnlt)) {

    //                     // Continue 1/4 cell more to connect at:
    //                     //
    //                     //       /
    //                     //      v
    //                     //     o

    //                     A.x -= 0.25; A.y += 0.25;
    //                 }
    //                 pathSet.insert(new Path(A, B));

    //                 // Continue the search from the end x+1,y-1
    //             } // lineContains
    //         }
    //     }
    // } // y


    // // Now look for curved corners. The syntax constraints require
    // // that these can always be identified by looking at three
    // // horizontally-adjacent characters.
    // for (var y = 0; y < grid.height; ++y) {
    //     for (var x = 0; x < grid.width; ++x) {
    //         var c = grid(x, y);

    //         // Note that because of undirected vertices, the
    //         // following cases are not exclusive
    //         if (isTopVertex(c)) {
    //             // -.
    //             //   |
    //             if (isSolidHLine(grid(x - 1, y)) && isSolidVLine(grid(x + 1, y + 1))) {
    //                 grid.setUsed(x - 1, y); grid.setUsed(x, y); grid.setUsed(x + 1, y + 1);
    //                 pathSet.insert(new Path(Vec2(x - 1, y), Vec2(x + 1, y + 1),
    //                                         Vec2(x + 1.1, y), Vec2(x + 1, y + 1)));
    //             }

    //             //  .-
    //             // |
    //             if (isSolidHLine(grid(x + 1, y)) && isSolidVLine(grid(x - 1, y + 1))) {
    //                 grid.setUsed(x - 1, y + 1); grid.setUsed(x, y); grid.setUsed(x + 1, y);
    //                 pathSet.insert(new Path(Vec2(x + 1, y), Vec2(x - 1, y + 1),
    //                                         Vec2(x - 1.1, y), Vec2(x - 1, y + 1)));
    //             }
    //         }

    //         // Special case patterns:
    //         //   .  .   .  .
    //         //  (  o     )  o
    //         //   '  .   '  '
    //         if (((c === ')') || isPoint(c)) && (grid(x - 1, y - 1) === '.') && (grid(x - 1, y + 1) === "\'")) {
    //             grid.setUsed(x, y); grid.setUsed(x - 1, y - 1); grid.setUsed(x - 1, y + 1);
    //             pathSet.insert(new Path(Vec2(x - 2, y - 1), Vec2(x - 2, y + 1),
    //                                     Vec2(x + 0.6, y - 1), Vec2(x + 0.6, y + 1)));
    //         }

    //         if (((c === '(') || isPoint(c)) && (grid(x + 1, y - 1) === '.') && (grid(x + 1, y + 1) === "\'")) {
    //             grid.setUsed(x, y); grid.setUsed(x + 1, y - 1); grid.setUsed(x + 1, y + 1);
    //             pathSet.insert(new Path(Vec2(x + 2, y - 1), Vec2(x + 2, y + 1),
    //                                     Vec2(x - 0.6, y - 1), Vec2(x - 0.6, y + 1)));
    //         }

    //         if (isBottomVertex(c)) {
    //             //   |
    //             // -'
    //             if (isSolidHLine(grid(x - 1, y)) && isSolidVLine(grid(x + 1, y - 1))) {
    //                 grid.setUsed(x - 1, y); grid.setUsed(x, y); grid.setUsed(x + 1, y - 1);
    //                 pathSet.insert(new Path(Vec2(x - 1, y), Vec2(x + 1, y - 1),
    //                                         Vec2(x + 1.1, y), Vec2(x + 1, y - 1)));
    //             }

    //             // |
    //             //  '-
    //             if (isSolidHLine(grid(x + 1, y)) && isSolidVLine(grid(x - 1, y - 1))) {
    //                 grid.setUsed(x - 1, y - 1); grid.setUsed(x, y); grid.setUsed(x + 1, y);
    //                 pathSet.insert(new Path(Vec2(x + 1, y), Vec2(x - 1, y - 1),
    //                                         Vec2(x - 1.1, y), Vec2(x - 1, y - 1)));
    //             }
    //         }

    //     } // for x
    // } // for y

    // // Find low horizontal lines marked with underscores. These
    // // are so simple compared to the other cases that we process
    // // them directly here without a helper function. Process these
    // // from top to bottom and left to right so that we can read
    // // them in a single sweep.
    // //
    // // Exclude the special case of double underscores going right
    // // into an ASCII character, which could be a source code
    // // identifier such as __FILE__ embedded in the diagram.
    // for (var y = 0; y < grid.height; ++y) {
    //     for (var x = 0; x < grid.width - 2; ++x) {
    //         var lt = grid(x - 1, y);

    //         if ((grid(x, y) === '_') && (grid(x + 1, y) === '_') &&
    //             (! isASCIILetter(grid(x + 2, y)) || (lt === '_')) &&
    //             (! isASCIILetter(lt) || (grid(x + 2, y) === '_'))) {

    //             var ltlt = grid(x - 2, y);
    //             var A = Vec2(x - 0.5, y + 0.5);

    //             if ((lt === '|') || (grid(x - 1, y + 1) === '|') ||
    //                 (lt === '.') || (grid(x - 1, y + 1) === "'")) {
    //                 // Extend to meet adjacent vertical
    //                 A.x -= 0.5;

    //                 // Very special case of overrunning into the side of a curve,
    //                 // needed for logic gate diagrams
    //                 if ((lt === '.') &&
    //                     ((ltlt === '-') ||
    //                      (ltlt === '.')) &&
    //                     (grid(x - 2, y + 1) === '(')) {
    //                     A.x -= 0.5;
    //                 }
    //             } else if (lt === '/') {
    //                 A.x -= 1.0;
    //             }

    //             // Detect overrun of a tight double curve
    //             if ((lt === '(') && (ltlt === '(') &&
    //                 (grid(x, y + 1) === "'") && (grid(x, y - 1) === '.')) {
    //                 A.x += 0.5;
    //             }
    //             lt = ltlt = undefined;

    //             do { grid.setUsed(x, y); ++x; } while (grid(x, y) === '_');

    //             var B = Vec2(x - 0.5, y + 0.5);
    //             var c = grid(x, y);
    //             var rt = grid(x + 1, y);
    //             var dn = grid(x, y + 1);

    //             if ((c === '|') || (dn === '|') || (c === '.') || (dn === "'")) {
    //                 // Extend to meet adjacent vertical
    //                 B.x += 0.5;

    //                 // Very special case of overrunning into the side of a curve,
    //                 // needed for logic gate diagrams
    //                 if ((c === '.') &&
    //                     ((rt === '-') || (rt === '.')) &&
    //                     (grid(x + 1, y + 1) === ')')) {
    //                     B.x += 0.5;
    //                 }
    //             } else if ((c === '\\')) {
    //                 B.x += 1.0;
    //             }

    //             // Detect overrun of a tight double curve
    //             if ((c === ')') && (rt === ')') && (grid(x - 1, y + 1) === "'") && (grid(x - 1, y - 1) === '.')) {
    //                 B.x += -0.5;
    //             }

    //             pathSet.insert(new Path(A, B));
    //         }
    //     } // for x
    // } // for y
} // findPaths
