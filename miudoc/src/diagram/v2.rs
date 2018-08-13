use std::convert::TryFrom;
use std::ops::Add;
use std::ops::Sub;
use std::u32::MAX;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Offset {
    o: u8,
}

//------------------------------------------------------------------------------
// Offset calculations

impl Offset {
    const DIVS: u8 = 20;
    const MAX: Offset = Offset{o: 19};
}

impl Add<Offset> for Offset {
    type Output = V2Elt;

    fn add(self, x: Offset) -> V2Elt {
        let z = self.o as u16 + x.o as u16;
        let base = (z / (Self::DIVS as u16)) as u32;
        let offset = Offset{o: (z % (Self::DIVS as u16)) as u8};
        V2Elt { base, offset }
    }
}

impl Sub<Offset> for Offset {
    type Output = D2Elt;

    fn sub(self, x: Offset) -> D2Elt {
        let z = self.o as i32 - x.o as i32;
        let base = z.div_euc(Self::DIVS as i32) as i64;
        let offset = Offset{o: z.mod_euc(Self::DIVS as i32) as u8};
        D2Elt { base, offset }
    }
}

//------------------------------------------------------------------------------
// Vector elements

type V2EltBase = u32;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct V2Elt {
    base: V2EltBase,
    offset: Offset,
}

pub trait IsV2Elt {
    fn to_v2elt(self) -> V2Elt;
}

impl IsV2Elt for V2EltBase {
    fn to_v2elt(self) -> V2Elt {
        V2Elt {base: self, offset: Offset{o: 0}}
    }
}

impl IsV2Elt for V2Elt {
    fn to_v2elt(self) -> V2Elt {
        self
    }
}

impl V2Elt {
    // pub fn checked_add(&self, v: V2Elt) -> Option<V2Elt> {
    //     let V2Elt{base: extra, offset} = self.offset.add(v.offset);
    //     let tmp1 = match self.base.checked_add(v.base) {
    //         Some(a) => a,
    //         None => { return None; }
    //     };
    //     // let base = self.base.checked_add(x.base).map(|a| a.checked_add(extra))
    //     panic!()
    //     // V2Elt { base, offset }
    // }
    const MAX: V2Elt = V2Elt {
        base: V2EltBase::MAX,
        offset: Offset::MAX,
    };

    pub fn force_sub(&self, v: V2Elt) -> V2Elt {
        let d = *self - v;
        assert!(0 <= d.base && d.base <= V2EltBase::MAX as D2EltBase);
        V2Elt { base: d.base as V2EltBase, offset: d.offset }
    }
}

impl Add<Offset> for V2Elt {
    type Output = V2Elt;
    fn add(self, x: Offset) -> V2Elt {
        panic!()
    }
}

impl Add<V2Elt> for V2Elt {
    type Output = V2Elt;

    fn add(self, x: V2Elt) -> V2Elt {
        let V2Elt{base: extra, offset} = self.offset + x.offset;
        let base = self.base.checked_add(x.base).unwrap().checked_add(extra).unwrap();
        V2Elt { base, offset }
    }
}

impl Sub<V2Elt> for V2Elt {
    type Output = D2Elt;

    fn sub(self, x: V2Elt) -> D2Elt {
        let D2Elt{base: extra, offset} = self.offset - x.offset;
        let base = self.base as D2EltBase - x.base as D2EltBase + extra;
        D2Elt { base, offset }
    }
}

type D2EltBase = i64;

pub struct D2Elt {
    base: D2EltBase,
    offset: Offset,
}

impl Sub for D2Elt {
    type Output = D2Elt;

    fn sub(self, x: D2Elt) -> D2Elt {
        let D2Elt{base: extra, offset} = self.offset - x.offset;
        let base = self.base.checked_sub(x.base).unwrap().checked_add(extra).unwrap();
        D2Elt { base, offset }
    }
}

impl From<V2Elt> for D2Elt {
    fn from(v: V2Elt) -> D2Elt {
        D2Elt {
            base: v.base.into(),
            offset: v.offset,
        }
    }
}

// Test that narrowing doesn't happen unless forced.
#[test]
fn elt_base_bounds_check() {
    assert!((D2EltBase::max as f64 - V2EltBase::max as f64).is_sign_positive());
    assert!((D2EltBase::min as f64 - V2EltBase::min as f64).is_sign_negative());
}

//------------------------------------------------------------------------------
// 2D vectors with small granular interpolation steps.


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct V2 {
    pub x: V2Elt,
    pub y: V2Elt,
}

// Ideally we should never wrap around so it is best to fail fast.
impl V2 {
    #[inline]
    pub fn rt_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 { x: self.x + n.to_v2elt(), y: self.y }
    }
    #[inline]
    pub fn lf_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 { x: self.x.force_sub(n.to_v2elt()), y: self.y }
    }
    #[inline]
    pub fn up_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 { x: self.x, y: self.y.force_sub(n.to_v2elt()) }
    }
    #[inline]
    pub fn dn_n<T: IsV2Elt>(&self, n: T) -> V2 {
        V2 { x: self.x, y: self.y + n.to_v2elt() }
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
            x: self.x.into() - other.x,
            y: self.y.into() - other.y,
        }
    }
}

/// Differences between two V2 values.
pub struct D2 {
    pub x: D2Elt,
    pub y: D2Elt,
}

impl From<V2> for D2 {
    fn from(v: V2) -> D2 {
        D2 {
            x: v.x.into(),
            y: v.y.into(),
        }
    }
}

impl D2 {
    pub fn force_into(&self) -> V2 {
        V2::try_from(*self).unwrap()
    }
}

impl TryFrom<D2> for V2 {
    type Error = ();
    fn try_from(d: D2) -> Result<V2, ()> {
        if d.x < V2Elt::MAX as D2Elt && d.y < V2Elt::MAX as D2Elt {
            Ok(V2 {
                x: d.x as V2Elt,
                y: d.y as V2Elt,
            })
        } else {
            Err(())
        }
    }
}

/// Trait implemented by tuples, V2 and D2.
pub trait IsV2 {
    fn to_v2(&self) -> V2;
}

impl IsV2 for (V2Elt, V2Elt) {
    fn to_v2(&self) -> V2 {
        V2 { x: self.0, y: self.1 }
    }
}

impl IsV2 for V2 {
    fn to_v2(&self) -> V2 {
        *self
    }
}
