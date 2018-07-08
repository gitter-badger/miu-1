use pos::Pos;

#[derive(Debug, Clone)]
pub struct Span<P> {
    pub start: P,
    pub end: P,
}

/// We don't handle files more than 4 GB in size.
pub type ByteOffset = u32;

pub type TokSpan = Span<ByteOffset>;

pub type FileSpan = Span<Pos>;
