// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#[derive(Clone, Debug)]
pub struct MString(String);

impl<'a> From<&'a str> for MString {
    #[inline(always)]
    fn from(s: &'a str) -> MString {
        MString(String::from(s))
    }
}
