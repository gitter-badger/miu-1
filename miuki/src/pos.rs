use std::ops::Add;

/// TODO: Decide semantics for the cases when a field is non-zero.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub col: u32,
    pub line: u32,
}

impl Add for Pos {
    type Output = Pos;
    #[inline(always)]
    fn add(self, x: Pos) -> Pos {
        Pos {
            col: self.col + x.col,
            line: self.line + x.line,
        }
    }
}

pub struct Col(u32);

impl Add<Col> for Pos {
    type Output = Pos;
    #[inline(always)]
    fn add(self, x: Col) -> Pos {
        Pos {
            col: self.col + x.0,
            line: self.line,
        }
    }
}
