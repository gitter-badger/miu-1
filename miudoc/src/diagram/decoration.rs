use diagram::primitives::*;

use std::iter;
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
    pub const DIVS: u16 = 360;
    pub const MAX: Angle = Angle { a: 359 };
    pub const ZERO: Angle = Angle { a: 0 };
    pub const A90: Angle = Angle { a: 90 };
    pub const A180: Angle = Angle { a: 180 };
    pub const A270: Angle = Angle { a: 270 };

    pub fn get(&self) -> u16 {
        self.a
    }

    pub fn ratio(n: u16, d: u16) -> Angle {
        let DIVS = Angle::DIVS;
        assert!(0 < n && n < d && d <= DIVS && DIVS % d == 0);
        Angle {a: DIVS/d * n}
    }
}

//------------------------------------------------------------------------------
// Decorations

#[derive(Debug, Hash, PartialEq, Eq)]
/// TODO: Replace this placeholder definition with something sensible.
pub struct Decoration {
    type_: char,
    /// Angle in degrees to rotate the thing by
    /// TODO: Define a better type for angle here.
    angle: u16,
}

impl Decoration {
    pub fn new(c: char, a: u16) -> Option<Decoration> {
        if is_decoration(c) {
            Some(Decoration {type_: c, angle: a})
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

pub struct DecorationSet {
    arrows: Vec<Decoration>,
    points: Vec<Decoration>,
}

impl DecorationSet {
    pub fn insert(&mut self, d: Decoration) {
        if is_point(d.type_()) {
            self.points.push(d)
        } else {
            self.arrows.push(d)
        }
    }
    pub fn iter<'a>(&'a self) -> DecorationSetIter<'a> {
        DecorationSetIter{iter: self.arrows.iter().chain(&self.points)}
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
