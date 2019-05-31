// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
