use std::convert::*;
use std::num::TryFromIntError;
use std::ops::*;

//------------------------------------------------------------------------------
// Offsets (grid unit subdivisions)

/// Offsets represent "in-between" values between adjacent points on the drawing
/// grid. This allows better control over the SVG -- even though the user's ASCII
/// picture cannot "write" there, we can make pictures tuned in a fine-grained
/// fashion by adjusting offsets.
///
/// Since Markdeep is in Javascript which only has one number type, it uses
/// floating point numbers (e.g. 0.5, 0.25) for offsets and uses an epsilon
/// value for comparisons. Our Offset type replaces that.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset {
    o: u8,
}

impl Offset {
    pub const DIVS: u8 = 20;
    pub const MAX: Offset = Offset { o: 19 };
    pub const ZERO: Offset = Offset { o: 0 };
    pub const QUARTER: Offset = Offset { o: 5 };
    pub const HALF: Offset = Offset { o: 10 };

    pub fn get(&self) -> u8 {
        self.o
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl Add<Offset> for Offset {
    type Output = V2Elt;

    fn add(self, x: Offset) -> V2Elt {
        let z = self.o as u16 + x.o as u16;
        let base = (z / (Self::DIVS as u16)) as u32;
        let offset = Offset {
            o: (z % (Self::DIVS as u16)) as u8,
        };
        V2Elt { base, offset }
    }
}

impl Sub<Offset> for Offset {
    type Output = D2Elt;

    fn sub(self, x: Offset) -> D2Elt {
        let z = self.o as i32 - x.o as i32;
        let base = z.div_euc(Self::DIVS as i32) as i64;
        let offset = Offset {
            o: z.mod_euc(Self::DIVS as i32) as u8,
        };
        D2Elt { base, offset }
    }
}

//------------------------------------------------------------------------------
// Vector elements

pub type V2EltBase = u32;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct V2Elt {
    base: V2EltBase,
    offset: Offset,
}

impl V2Elt {
    pub const MAX: V2Elt = V2Elt {
        base: u32::min_value(),
        offset: Offset::MAX,
    };

    pub fn force_sub(&self, v: V2Elt) -> V2Elt {
        let d = *self - v;
        assert!(0 <= d.base && d.base <= <V2EltBase>::max_value() as D2EltBase);
        V2Elt {
            base: d.base as V2EltBase,
            offset: d.offset,
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    pub fn round(&self) -> usize {
        let extra = if self.offset.get() < Offset::DIVS / 2 { 0 } else { 1 };
        self.base as usize + extra
    }
}

//----------------------------------------------------------
// Conversions

// Rust inference defaults to i32 literals when the type is under-constrained
// (see https://doc.rust-lang.org/reference/tokens.html#number-literals),
// which means that if we write a generic implementation like:
//
// impl<T: Into<V2EltBase>> IsV2Elt for T {
//     fn to_v2elt(self) -> V2Elt {
//         V2Elt {
//             base: self.into(),
//             offset: Offset { o: 0 },
//         }
//     }
// }
//
// then code can start failing with unannotated literals as there is no
// instance Into<u32> for i32.
impl From<V2EltBase> for V2Elt {
    fn from(b: V2EltBase) -> V2Elt {
        V2Elt {
            base: b,
            offset: Offset { o: 0 },
        }
    }
}

impl TryFrom<usize> for V2Elt {
    type Error = TryFromIntError;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn try_from(x: usize) -> Result<V2Elt, Self::Error> {
        V2EltBase::try_from(x).map(|base| V2Elt { base, offset: Offset::ZERO })
    }
}

impl TryFrom<D2Elt> for V2Elt {
    type Error = TryFromIntError;

    fn try_from(x: D2Elt) -> Result<V2Elt, Self::Error> {
        V2EltBase::try_from(x.base).map(|base| V2Elt {
            base,
            offset: x.offset,
        })
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl Add<Offset> for V2Elt {
    type Output = V2Elt;
    fn add(self, _x: Offset) -> Self::Output {
        unimplemented!()
    }
}

impl AddAssign<Offset> for V2Elt {
    fn add_assign(&mut self, x: Offset) {
        *self = *self + x;
    }
}

// I don't really like this implementation but I'm trying to stick to
// Markdeep's logic closely, so shit is what it is...
impl Sub<Offset> for V2Elt {
    type Output = V2Elt;
    fn sub(self, x: Offset) -> Self::Output {
        let D2Elt {
            base: extra,
            offset,
        } = self.offset - x;
        let base = self.base as D2EltBase + extra;
        (D2Elt { base, offset }).try_into().unwrap()
    }
}

impl SubAssign<Offset> for V2Elt {
    fn sub_assign(&mut self, x: Offset) {
        *self = *self - x;
    }
}

impl Add<V2Elt> for V2Elt {
    type Output = V2Elt;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn add(self, x: V2Elt) -> V2Elt {
        let V2Elt{base: extra, offset} = self.offset + x.offset;
        let base = self.base
            .checked_add(x.base).unwrap()
            .checked_add(extra).unwrap();
        V2Elt { base, offset }
    }
}

impl Sub<V2Elt> for V2Elt {
    type Output = D2Elt;

    fn sub(self, x: V2Elt) -> D2Elt {
        let D2Elt {
            base: extra,
            offset,
        } = self.offset - x.offset;
        let base = self.base as D2EltBase - x.base as D2EltBase + extra;
        D2Elt { base, offset }
    }
}

//------------------------------------------------------------------------------
// Difference vector elements

pub type D2EltBase = i64;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct D2Elt {
    base: D2EltBase,
    offset: Offset,
}

//----------------------------------------------------------
// Conversions

impl From<V2Elt> for D2Elt {
    fn from(v: V2Elt) -> D2Elt {
        D2Elt {
            base: v.base.into(),
            offset: v.offset,
        }
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl AddAssign for D2Elt {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn add_assign(&mut self, d: D2Elt) {
        let v = self.offset + d.offset;
        self.offset = v.offset;
        self.base = self.base.checked_add(d.base).unwrap()
            .checked_add(v.base as D2EltBase).unwrap();
    }
}

impl Sub for D2Elt {
    type Output = D2Elt;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn sub(self, x: D2Elt) -> D2Elt {
        let D2Elt{base: extra, offset} = self.offset - x.offset;
        let base = self.base
            .checked_sub(x.base).unwrap()
            .checked_add(extra).unwrap();
        D2Elt { base, offset }
    }
}

//----------------------------------------------------------
// Tests

// Test that narrowing doesn't happen unless forced.
#[test]
fn elt_base_bounds_check() {
    assert!((D2EltBase::max as f64 - V2EltBase::max as f64).is_sign_positive());
    assert!((D2EltBase::min as f64 - V2EltBase::min as f64).is_sign_negative());
}

//------------------------------------------------------------------------------
// 2D vectors

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct V2 {
    pub x: V2Elt,
    pub y: V2Elt,
}

// Ideally we should never wrap around so it is best to fail fast.
impl V2 {
    #[inline]
    pub fn rt_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 {
            x: self.x + n.to_v2elt(),
            y: self.y,
        }
    }
    #[inline]
    pub fn lf_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 {
            x: self.x.force_sub(n.to_v2elt()),
            y: self.y,
        }
    }
    #[inline]
    pub fn up_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 {
            x: self.x,
            y: self.y.force_sub(n.to_v2elt()),
        }
    }
    #[inline]
    pub fn dn_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 {
            x: self.x,
            y: self.y + n.to_v2elt(),
        }
    }
    #[inline]
    pub fn rt(&self) -> V2 {
        self.rt_n(1)
    }
    #[inline]
    pub fn lf(&self) -> V2 {
        self.lf_n(1)
    }
    #[inline]
    pub fn up(&self) -> V2 {
        self.up_n(1)
    }
    #[inline]
    pub fn dn(&self) -> V2 {
        self.dn_n(1)
    }
}

//----------------------------------------------------------
// Conversions

impl TryFrom<D2> for V2 {
    type Error = ();
    fn try_from(d: D2) -> Result<V2, ()> {
        if d.x < V2Elt::MAX.into() && d.y < V2Elt::MAX.into() {
            Ok(d.force_into())
        } else {
            Err(())
        }
    }
}

//----------------------------------------------------------
// Arithmetic operations

impl Add for V2 {
    type Output = V2;

    fn add(self, other: V2) -> V2 {
        V2 {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub<D2> for V2 {
    type Output = D2;

    fn sub(self, other: D2) -> D2 {
        D2 {
            x: D2Elt::from(self.x) - other.x,
            y: D2Elt::from(self.y) - other.y,
        }
    }
}

//------------------------------------------------------------------------------
// 2D difference vectors

/// Differences between two V2 values.
pub struct D2 {
    pub x: D2Elt,
    pub y: D2Elt,
}

impl D2 {
    pub fn force_into(self) -> V2 {
        V2::try_from(self).unwrap()
    }
}

//----------------------------------------------------------
// Conversions

impl From<V2> for D2 {
    fn from(v: V2) -> D2 {
        D2 {
            x: v.x.into(),
            y: v.y.into(),
        }
    }
}

//------------------------------------------------------------------------------
// Conversion traits

/// Trait equivalent to Into<V2Elt> because
///
///     pub trait IsV2Elt = Into<V2Elt>;
///
/// doesn't compile because trait aliases haven't been implemented yet.
/// The trait method also has a more descriptive name.
pub trait IsV2Elt {
    fn to_v2elt(&self) -> V2Elt;
}

impl IsV2Elt for Offset {
    fn to_v2elt(&self) -> V2Elt {
        V2Elt {
            base: 0,
            offset: self.clone(),
        }
    }
}

impl IsV2Elt for V2EltBase {
    fn to_v2elt(&self) -> V2Elt {
        V2Elt {
            base: self.clone(),
            offset: Offset { o: 0 },
        }
    }
}

impl IsV2Elt for V2Elt {
    fn to_v2elt(&self) -> V2Elt {
        *self
    }
}

/// Trait equivalent to Into<V2> because
///
///     pub trait IsV2 = Into<V2>;
///
/// doesn't compile because trait aliases haven't been implemented yet.
/// The trait method also has a more descriptive name.
pub trait IsV2 {
    fn to_v2(&self) -> V2;
}

impl<T: IsV2Elt> IsV2 for (T, T) {
    fn to_v2(&self) -> V2 {
        V2 {
            x: self.0.to_v2elt(),
            y: self.1.to_v2elt(),
        }
    }
}

impl IsV2 for V2 {
    fn to_v2(&self) -> V2 {
        *self
    }
}
