
use diagram::v2::*;

use std::collections::HashSet;
use std::collections::hash_set;

#[derive(Debug, PartialEq, Eq, Hash)]
/// The field names have been kept short for easy comparison with the Markdeep
/// source code.
pub struct Path {
    /// Starting point for the path.
    pub a: V2,
    /// Ending point for the path.
    pub b: V2,
    /// A control point for the Bezier curve.
    pub c: Option<V2>,
    /// A control point for the Bezier curve.
    pub d: Option<V2>,
    pub dashed: bool,
}

pub const EPSILON: f64 = 1E-6;

#[cfg_attr(rustfmt, rustfmt_skip)]
impl Path {
    pub fn is_vertical(&self) -> bool {
        self.b.x == self.a.x
    }

    pub fn is_horizontal(&self) -> bool {
        self.b.y == self.a.y
    }

    pub fn is_diagonal(&self) -> bool {
        let d = self.b - D2::from(self.a);
        ((d.y + d.x) as f64) < EPSILON
    }

    pub fn is_backdiag(&self) -> bool {
        let d = self.b - D2::from(self.a);
        (D2Elt::abs(d.y - d.x) as f64) < EPSILON
    }

    pub fn is_curved(&self) -> bool {
        self.c.is_some()
    }

    pub fn ends_at(&self, v: V2) -> bool {
        self.a == v || self.b == v
    }

    pub fn up_ends_at(&self, v: V2) -> bool {
        self.is_vertical()
        && self.a.x == v.x
        && V2Elt::min(self.a.y, self.b.y) == v.y
    }

    pub fn down_ends_at(&self, v: V2) -> bool {
        self.is_vertical()
        && self.a.x == v.x
        && V2Elt::max(self.a.y, self.b.y) == v.y
    }

    pub fn left_ends_at(&self, v: V2) -> bool {
        self.is_horizontal()
        && self.a.y == v.y
        && V2Elt::min(self.a.x, self.b.x) == v.x
    }

    pub fn right_ends_at(&self, v: V2) -> bool {
        self.is_horizontal()
        && self.a.y == v.y
        && V2Elt::max(self.a.x, self.b.x) == v.x
    }

    pub fn diagonal_up_ends_at(&self, v: V2) -> bool {
        self.is_diagonal()
        && if self.a.y < self.b.y { self.a == v }
           else { self.b == v }
    }

    pub fn diagonal_down_ends_at(&self, v: V2) -> bool {
        self.is_diagonal()
        && if self.b.y < self.a.y { self.a == v }
           else { self.b == v }
    }

    pub fn backdiag_up_ends_at(&self, v: V2) -> bool {
        self.is_backdiag() &&
        if self.a.y < self.b.y { self.a == v }
        else { self.b == v }
    }

    pub fn backdiag_down_ends_at(&self, v: V2) -> bool {
        self.is_backdiag() &&
        if self.b.y < self.a.y { self.a == v }
        else { self.b == v }
    }

    pub fn vertical_passes_thru(&self, v: V2) -> bool {
        self.is_vertical()
        && self.a.y == v.y
        && V2Elt::min(self.a.y, self.b.y) <= v.y
        && V2Elt::max(self.a.y, self.b.y) >= v.y
    }

    pub fn horizontal_passes_thru(&self, v: V2) -> bool {
        self.is_horizontal()
        && self.a.x == v.x
        && V2Elt::min(self.a.x, self.b.x) <= v.x
        && V2Elt::max(self.a.x, self.b.x) >= v.x
    }

    pub fn to_svg(&self) -> String {
        unimplemented!()
    }
}

pub struct PathSet {
    set: HashSet<Path>,
}

impl PathSet {
    pub fn insert(&mut self, p: Path) {
        self.set.insert(p);
    }
    pub fn iter<'a>(&'a self) -> PathSetIter<'a> {
        PathSetIter{iter: self.set.iter()}
    }
}

pub struct PathSetIter<'a> {
    iter: hash_set::Iter<'a, Path>,
}

impl<'a> Iterator for PathSetIter<'a> {
    type Item = &'a Path;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
