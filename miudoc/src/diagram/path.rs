
use diagram::v2::*;

use std::collections::HashSet;
use std::collections::hash_set;

/// The field names have been kept short for easy comparison with the Markdeep
/// source code.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Path {
    /// Starting point for the path.
    a: V2,
    /// Ending point for the path.
    b: V2,
    /// A control point for the Bezier curve.
    c: Option<V2>,
    /// A control point for the Bezier curve.
    d: Option<V2>,
    /// `- - -` vs `-----`.
    dashed: bool,
}

// Don't use EPSILON because we have proper types, not just number.
// pub const EPSILON: f64 = 1E-6;

#[cfg_attr(rustfmt, rustfmt_skip)]
impl Path {
    pub fn straight(a: V2, b: V2) -> Path {
        Path { a, b, c: None, d: None, dashed: false }
    }

    pub fn curved(a: V2, b: V2, c: V2, d: V2) -> Path {
        Path { a, b, c: Some(c), d: Some(d), dashed: false }
    }

    pub fn is_vertical(&self) -> bool {
        self.b.x == self.a.x
    }

    pub fn is_horizontal(&self) -> bool {
        self.b.y == self.a.y
    }

    pub fn is_diagonal(&self) -> bool {
        let d = self.b - D2::from(self.a);
        // Don't use EPSILON because we have proper types, not just number.
        // ((d.y + d.x) as f64) < EPSILON
        d.x + d.y == D2Elt::ZERO
    }

    pub fn is_backdiag(&self) -> bool {
        let d = self.b - D2::from(self.a);
        // Don't use EPSILON because we have proper types, not just number.
        // (D2Elt::abs(d.y - d.x) as f64) < EPSILON
        d.x == d.y
    }

    pub fn is_curved(&self) -> bool {
        self.c.is_some()
    }

    pub fn ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.a == v || self.b == v
    }

    pub fn up_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_vertical()
        && self.a.x == v.x
        && V2Elt::min(self.a.y, self.b.y) == v.y
    }

    pub fn dn_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_vertical()
        && self.a.x == v.x
        && V2Elt::max(self.a.y, self.b.y) == v.y
    }

    pub fn lf_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_horizontal()
        && self.a.y == v.y
        && V2Elt::min(self.a.x, self.b.x) == v.x
    }

    pub fn rt_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_horizontal()
        && self.a.y == v.y
        && V2Elt::max(self.a.x, self.b.x) == v.x
    }

    pub fn diagonal_up_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_diagonal()
        && if self.a.y < self.b.y { self.a == v }
           else { self.b == v }
    }

    pub fn diagonal_dn_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_diagonal()
        && if self.b.y < self.a.y { self.a == v }
           else { self.b == v }
    }

    pub fn backdiag_up_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_backdiag() &&
        if self.a.y < self.b.y { self.a == v }
        else { self.b == v }
    }

    pub fn backdiag_dn_ends_at<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_backdiag() &&
        if self.b.y < self.a.y { self.a == v }
        else { self.b == v }
    }

    pub fn vertical_passes_thru<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_vertical()
        && self.a.y == v.y
        && V2Elt::min(self.a.y, self.b.y) <= v.y
        && V2Elt::max(self.a.y, self.b.y) >= v.y
    }

    pub fn horizontal_passes_thru<T: IsV2>(&self, v: T) -> bool {
        let v = v.to_v2();
        self.is_horizontal()
        && self.a.x == v.x
        && V2Elt::min(self.a.x, self.b.x) <= v.x
        && V2Elt::max(self.a.x, self.b.x) >= v.x
    }

    pub fn to_svg(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct PathSet {
    set: HashSet<Path>,
}

impl PathSet {
    pub fn new() -> PathSet {
        PathSet { set: HashSet::new() }
    }
    pub fn insert(&mut self, p: Path) {
        self.set.insert(p);
    }
    pub fn iter<'a>(&'a self) -> PathSetIter<'a> {
        PathSetIter{iter: self.set.iter()}
    }
    // TODO: See if these methods can be generated using a macro.
    pub fn up_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.up_ends_at(v))
    }
    pub fn dn_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.dn_ends_at(v))
    }
    pub fn lf_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.lf_ends_at(v))
    }
    pub fn rt_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.rt_ends_at(v))
    }
    pub fn diagonal_up_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.diagonal_up_ends_at(v))
    }
    pub fn diagonal_dn_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.diagonal_dn_ends_at(v))
    }
    pub fn backdiag_up_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.backdiag_up_ends_at(v))
    }
    pub fn backdiag_dn_ends_at<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.backdiag_dn_ends_at(v))
    }
    pub fn horizontal_passes_thru<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.horizontal_passes_thru(v))
    }
    pub fn vertical_passes_thru<T: IsV2 + Copy>(&self, v: T) -> bool {
        self.iter().any(|p| p.vertical_passes_thru(v))
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
