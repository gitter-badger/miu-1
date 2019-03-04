// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::primitives::*;
use super::v2::{V2, IsV2, V2Elt};

use std::iter;
use std::slice;
use std::ops::Add;
use std::ops::Sub;

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

    pub fn get(&self) -> u16 {
        self.a
    }

    pub fn ratio(n: u16, d: u16) -> Angle {
        let divs = Angle::DIVS;
        assert!(0 < n && n < d && d <= divs && divs % d == 0);
        Angle {a: divs/d * n}
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl Add for Angle {
    type Output = Angle;
    fn add(self, other: Angle) -> Angle {
        Angle {a: self.get().wrapping_add(other.get()).rem_euclid(Angle::DIVS)}
    }
}

impl Sub for Angle {
    type Output = Angle;
    fn sub(self, other: Angle) -> Angle {
        Angle {a: self.get().wrapping_sub(other.get()).rem_euclid(Angle::DIVS)}
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
            Some(Decoration {pos: v.to_v2(), type_: c, angle: a})
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

impl DecorationSet {
    pub fn new() -> DecorationSet {
        DecorationSet { arrows: vec![], points: vec![] }
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
