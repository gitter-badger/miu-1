// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::primitives::*;
use super::to_svg::ToSvg;
use super::v2::{IsV2, IsV2Elt, V2Elt, V2};

use svg::node::element;

use std::iter;
use std::ops::Add;
use std::ops::Sub;
use std::slice;

//------------------------------------------------------------------------------
// Angles

/// Angles in degrees.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Angle {
    a: u16,
}

// TODO: A lot of the code in here is shared with v2::Offset.
// Once const generics land, see if we can use a common modular type.
impl Angle {
    /// For consistency with Offset
    pub const DIVS: u16 = 360;
    pub const MAX: Angle = Angle { a: 359 };
    pub const ZERO: Angle = Angle { a: 0 };

    // This is tied to the aspect ratio; there is a test locking down the
    // relation. I've separated it out into a constant so that it can be
    // used like other angles.
    pub const DIAGONAL: Angle = Angle { a: 26 };

    pub const A0: Angle = Angle { a: 0 };
    pub const A90: Angle = Angle { a: 90 };
    pub const A180: Angle = Angle { a: 180 };
    pub const A270: Angle = Angle { a: 270 };

    pub fn ratio(n: u16, d: u16) -> Angle {
        let divs = Angle::DIVS;
        assert!(0 < n && n < d && d <= divs && divs % d == 0);
        Angle { a: divs / d * n }
    }

    pub fn to_degrees(&self) -> f64 {
        self.a as f64
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl Add for Angle {
    type Output = Angle;
    fn add(self, other: Angle) -> Angle {
        Angle {
            a: self.a.wrapping_add(other.a).rem_euclid(Angle::DIVS),
        }
    }
}

impl Sub for Angle {
    type Output = Angle;
    fn sub(self, other: Angle) -> Angle {
        Angle {
            a: self.a.wrapping_sub(other.a).rem_euclid(Angle::DIVS),
        }
    }
}

//------------------------------------------------------------------------------
// Decorations

#[derive(Debug, Hash, PartialEq, Eq)]
/// TODO: Replace this placeholder definition with something sensible.
pub struct Decoration {
    pos: V2,
    type_: char,
    /// Angle in degrees to rotate the thing by
    /// TODO: Define a better type for angle here.
    angle: Angle,
}

impl Decoration {
    pub fn new<T: IsV2>(v: T, c: char, a: Angle) -> Option<Decoration> {
        if is_decoration(c) {
            Some(Decoration {
                pos: v.to_v2(),
                type_: c,
                angle: a,
            })
        } else {
            None
        }
    }
    pub fn type_(&self) -> char {
        self.type_
    }
}

//------------------------------------------------------------------------------
// Decoration sets

#[derive(Debug)]
pub struct DecorationSet {
    arrows: Vec<Decoration>,
    points: Vec<Decoration>,
}

#[derive(Clone, Debug)]
pub enum TaggedNode {
    Rectangle(element::Rectangle),
    Circle(element::Circle),
    Path(element::Path),
    Polygon(element::Polygon),
}

use self::TaggedNode::{*};

impl std::fmt::Display for TaggedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Rectangle(r) => r.fmt(f),
            Circle(c) => c.fmt(f),
            Path(p) => p.fmt(f),
            Polygon(p) => p.fmt(f),
        }
    }
}

use svg::node::Node;

impl Node for TaggedNode {
    fn append<T>(&mut self, t: T)
        where T: Node
    {
        match self {
            Rectangle(r) => r.append(t),
            Circle(c) => c.append(t),
            Path(p) => p.append(t),
            Polygon(p) => p.append(t),
        }
    }
    fn assign<T, U>(&mut self, t: T, u: U)
    where T: Into<String>, U: Into<svg::node::Value>
    {
        match self {
            Rectangle(r) => r.assign(t, u),
            Circle(c) => c.assign(t, u),
            Path(p) => p.assign(t, u),
            Polygon(p) => p.assign(t, u),
        }
    }
}

impl ToSvg for Decoration {
    type Output = TaggedNode;

    #[allow(unused_comparisons)]
    fn to_svg(&self) -> Self::Output {
        use super::v2::{Offset, V2EltBase};
        use super::{ASPECT, SCALE, STROKE_WIDTH};

        impl V2 {
            fn x_svg(&self) -> f64 {
                self.x.to_f64() * SCALE as f64
            }
            fn y_svg(&self) -> f64 {
                self.y.to_f64() * (SCALE * ASPECT) as f64
            }
            fn to_svg(&self) -> String {
                format!("{},{}", self.x_svg(), self.y_svg())
            }
        }
        // Corresponds to C in Markdeep
        let decoration = self;
        let pos = decoration.pos;
        if is_jump(decoration.type_) {
            // [MM] Slide jumps
            let dx = |p: super::v2::V2| {
                let tq = Offset::THREE_QUARTER;
                if decoration.type_ == ')' {
                    p.rt_n(tq)
                } else {
                    p.lf_n(tq)
                }
            };
            let up = pos.up_n(Offset::HALF);
            let dn = pos.dn_n(Offset::HALF);
            let side_up = dx(up);
            let side_dn = dx(dn);
            let d = element::path::Data::new()
                .move_to(dn.as_tuple())
                .cubic_curve_to(side_dn.as_tuple())
                .cubic_curve_to(side_up.as_tuple())
                .cubic_curve_to(up.as_tuple());
            let p = element::Path::new().set("fill", "none").set("d", d);
            Path(p)
        } else if is_point(decoration.type_) {
            let cls = if decoration.type_ == '*' {
                "closeddot"
            } else {
                "opendot"
            };
            let c = element::Circle::new()
                .set("cx", pos.x_svg())
                .set("cy", pos.y_svg())
                .set("r", SCALE - STROKE_WIDTH)
                .set("class", cls);
            Circle(c)
        } else if let Some(i) = is_gray_at(decoration.type_) {
            let shade = ((&GRAY_CHARS.len() - 1 - i) as f64 * 63.75).round();
            let pos = pos.lf_n(Offset::HALF).up_n(Offset::HALF);
            let r = element::Rectangle::new()
                .set("x", pos.x_svg())
                .set("y", pos.y_svg())
                .set("width", SCALE)
                .set("height", SCALE * ASPECT)
                .set("style", "stroke:none")
                .set("fill", format!("rgb({0},{0},{0})", shade));
            Rectangle(r)
        } else if let Some(i) = is_tri_at(decoration.type_) {
            // [MM] 30-60-90 triangle
            // TODO: Finish porting triangle code.
            assert!(0 <= i && i <= 3);
            let i = i as V2EltBase;
            let ys = (-(i >> 1)).to_v2elt() + Offset::HALF.to_v2elt();
            let xs = if ys.signum() > 0 {
                -(i & 1)
            } else {
                -1 + (i & 1)
            }
            .to_v2elt()
                + Offset::HALF.to_v2elt();
            let tip = (pos.x + xs, pos.y.force_sub(ys)).to_v2();
            let up = (pos.x + xs, pos.y + ys).to_v2();
            let dn = (pos.x.force_sub(ys), pos.y + ys).to_v2();
            let p = svg::node::element::Polygon::new()
                .set(
                    "points",
                    format!("{} {} {}", tip.to_svg(), up.to_svg(), dn.to_svg()),
                )
                .set("style", "stroke:none");
            Polygon(p)
        } else {
            assert!(is_arrow_head(decoration.type_));
            let tip = pos.rt();
            let up = (pos.x - Offset::HALF, pos.y - Offset::ratio(7, 20)).to_v2();
            let dn = (pos.x - Offset::HALF, pos.y + Offset::ratio(7, 20)).to_v2();
            let p = svg::node::element::Polygon::new()
                .set(
                    "points",
                    format!("{} {} {}", tip.to_svg(), up.to_svg(), dn.to_svg()),
                )
                .set("style", "stroke:none")
                .set(
                    "transform",
                    format!("rotate({},{})", decoration.angle.to_degrees(), pos.to_svg()),
                );
            Polygon(p)
        }
    }
}

// impl ToSvg for DecorationSet {
//     type Output = Vec<Box<dyn svg::node::Node>>;
//     fn to_svg(&self) -> Self::Output {

//         let mut doc: <Self as ToSvg>::Output = vec![];

//         for decoration in self.arrows.iter().chain(self.points.iter()) {}
//         doc
//     }
// }

impl DecorationSet {
    pub fn new() -> DecorationSet {
        DecorationSet {
            arrows: vec![],
            points: vec![],
        }
    }
    pub fn insert<T: IsDecoration>(&mut self, d: T) {
        let d = d.to_decoration();
        if is_point(d.type_()) {
            self.points.push(d)
        } else {
            self.arrows.push(d)
        }
    }
    pub fn iter<'a>(&'a self) -> DecorationSetIter<'a> {
        DecorationSetIter {
            iter: self.arrows.iter().chain(&self.points),
        }
    }
}

pub struct DecorationSetIter<'a> {
    iter: iter::Chain<slice::Iter<'a, Decoration>, slice::Iter<'a, Decoration>>,
}

impl<'a> Iterator for DecorationSetIter<'a> {
    type Item = &'a Decoration;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

//------------------------------------------------------------------------------
// Conversion trait(s)

pub trait IsDecoration {
    #[inline]
    fn to_decoration(&self) -> Decoration;
}

impl<T: IsV2> IsDecoration for (T, char) {
    fn to_decoration(&self) -> Decoration {
        Decoration::new(self.0.to_v2(), self.1, Angle::ZERO).unwrap()
    }
}

impl<T: IsV2> IsDecoration for (T, char, Angle) {
    fn to_decoration(&self) -> Decoration {
        Decoration::new(self.0.to_v2(), self.1, self.2).unwrap()
    }
}

impl IsDecoration for (V2Elt, V2Elt, char, Angle) {
    fn to_decoration(&self) -> Decoration {
        Decoration::new((self.0, self.1).to_v2(), self.2, self.3).unwrap()
    }
}
