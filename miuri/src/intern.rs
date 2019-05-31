// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

#[derive(Copy, Clone)]
pub struct InternedStr {
    index: u32,
    start: u16,
    len: u16,
}

/// An alternative to &str.
///
/// It is guaranteed that the first byte of the struct will be 0.
/// To make sure this invariant holds, we keep the len field private.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct StringRef {
    len: usize,
    pub ptr: *const u8,
}

impl StringRef {
    pub const LEN_BYTES: usize = std::mem::size_of::<usize>();
    pub const MAX_LEN: usize = usize::max_value() >> 8;
    pub const SIZE: usize = std::mem::size_of::<Self>();

    #[cfg(target_endian = "little")]
    pub unsafe fn new_unchecked(s: &[u8]) -> StringRef {
        std::debug_assert!(s.len() <= Self::MAX_LEN);
        StringRef {
            len: s.len() << 8,
            ptr: s.as_ptr(),
        }
    }

    #[cfg(target_endian = "big")]
    pub unsafe fn new_unchecked(s: &[u8]) -> StringRef {
        std::debug_assert!(s.len() <= MAX_LEN);
        StringRef {
            len: s.len(),
            ptr: s.as_ptr(),
        }
    }

    #[cfg(target_endian = "little")]
    pub fn len(&self) -> usize {
        self.len >> 8
    }

    #[cfg(target_endian = "big")]
    pub fn len(&self) -> usize {
        self.len
    }

    pub unsafe fn as_str<'a>(self) -> &'a [u8] {
        std::slice::from_raw_parts(self.ptr, self.len())
    }
}

/// The possible states are as follows:
///
/// 1. Empty string  -> union is fully zeroed out.
/// 2. Local data    -> first byte is non-zero and string is null-terminated.
/// 3. Nonlocal data -> first byte is zero, first word is length, second is ptr.
///
/// If the string to be stored contains nulls, we can't stored it locally, even
/// if it is small.
#[repr(C)]
union SSOStringRefInner {
    sref: StringRef,
    bytes: [u8; StringRef::SIZE],
}

struct SSOStringRef {
    inner: SSOStringRefInner,
}

impl SSOStringRef {
    pub unsafe fn new_unchecked(s: &[u8]) -> Option<SSOStringRef> {
        if s.len() == 0 {
            Some(SSOStringRef {
                inner: SSOStringRefInner {
                    bytes: [0; StringRef::SIZE],
                },
            })
        } else if s.len() <= StringRef::SIZE && s.iter().all(|&c| c != 0) {
            let mut r = SSOStringRef {
                inner: SSOStringRefInner {
                    bytes: [0; StringRef::SIZE],
                },
            };
            for (i, &c) in s.iter().enumerate() {
                r.inner.bytes[i] = c;
            }
            Some(r)
        } else if s.len() > StringRef::MAX_LEN {
            None
        } else {
            Some(SSOStringRef {
                inner: SSOStringRefInner {
                    sref: StringRef::new_unchecked(s),
                },
            })
        }
    }

    pub fn new(s: &str) -> Option<SSOStringRef> {
        unsafe { Self::new_unchecked(s.as_bytes()) }
    }

    unsafe fn has_inline_data(&self) -> bool {
        self.inner.bytes[0] != 0
    }

    pub unsafe fn as_str<'a>(&'a self) -> &'a str {
        let s: &'a [u8];
        if self.has_inline_data() {
            let mut i = 1;
            while i < StringRef::SIZE && self.inner.bytes[i] != 0 {
                i += 1;
            }
            s = self.inner.bytes.get(0..i).unwrap()
        } else {
            s = self.inner.sref.clone().as_str()
        }
        std::str::from_utf8_unchecked(s)
    }
}

impl PartialEq for SSOStringRef {
    fn eq(&self, other: &SSOStringRef) -> bool {
        for i in 0..StringRef::LEN_BYTES {
            if unsafe { self.inner.bytes[i] != other.inner.bytes[i] } {
                return false;
            }
        }
        // Lengths are now bitwise equal.
        unsafe {
            if self.inner.sref.len() == 0 {
                return true;
            } else if self.has_inline_data() {
                // If the first string has inline data, then so does the second one,
                // otherwise we would've already returned false.
                //
                // Even though we don't have pointers, instead of doing byte-wise
                // comparison, we can treat the rest of the data as pointers and
                // compare the two pointers.
                std::debug_assert!(other.has_inline_data());
                return self.inner.sref.ptr == other.inner.sref.ptr;
            } else {
                // Now we really have two pointers on our hands
                return (self.inner.sref.ptr == other.inner.sref.ptr)
                    || (self.inner.sref.as_str() == other.inner.sref.as_str());
            }
        }
    }
}

impl Eq for SSOStringRef {}

impl Hash for SSOStringRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (unsafe { self.as_str() }).hash(state);
    }
}

const INDEX_SENTINEL_VALUE: u32 = u32::max_value();

const STORAGE_CHUNK_SIZE: usize = u16::max_value() as usize;

pub struct Interner {
    indices: HashMap<SSOStringRef, InternedStr>,
    bytes_left: usize,
    storage: Vec<[u8; STORAGE_CHUNK_SIZE]>,
    huge_strings: Vec<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum InternError {
    StringTooLong,
}

impl Interner {
    pub fn new(ss: &[&str]) -> Self {
        let mut i = Self::empty();
        for s in ss.iter() {
            i.insert(s).unwrap();
        }
        i
    }

    pub fn empty() -> Self {
        Interner {
            indices: HashMap::new(),
            bytes_left: STORAGE_CHUNK_SIZE,
            storage: vec![],
            huge_strings: vec![],
        }
    }

    pub fn get_str_unchecked<'a, 'b: 'a>(
        &'b self,
        istr: InternedStr,
    ) -> &'a str {
        let start = istr.start as usize;
        if istr.index >= INDEX_SENTINEL_VALUE {
            self.huge_strings[start].as_str()
        } else {
            let bytes = self.storage[istr.index as usize]
                .get(start..(start + istr.len as usize))
                .unwrap();
            unsafe { std::str::from_utf8_unchecked(bytes) }
        }
    }

    pub fn get_str<'a, 'b: 'a>(&'b self, istr: InternedStr) -> Option<&'a str> {
        let start = istr.start as usize;
        if istr.index >= INDEX_SENTINEL_VALUE {
            Some(self.huge_strings[start].as_str())
        } else {
            let bytes = self.storage[istr.index as usize]
                .get(start..(start + istr.len as usize))
                .unwrap();
            std::str::from_utf8(bytes).ok()
        }
    }

    pub fn insert(&mut self, s: &str) -> Result<InternedStr, InternError> {
        let sref = match SSOStringRef::new(s) {
            Some(x) => x,
            None => {
                return Err(InternError::StringTooLong);
            }
        };

        let mut h = self.indices.hasher().build_hasher();
        // It is fine to do this as the implemetation for hashing a StringRef
        // converts it to a &str anyways before hashing it.
        s.hash(&mut h);
        let hash_value = h.finish();
        let raw_ent = self
            .indices
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash_value, &sref);

        use std::collections::hash_map::RawEntryMut;

        match raw_ent {
            RawEntryMut::Occupied(o) => Ok(*o.get()),
            RawEntryMut::Vacant(v) => {
                let istr: InternedStr;
                let new_key: SSOStringRef;
                if s.len() <= self.bytes_left {
                    let si = self.storage.len() - 1;
                    std::debug_assert!(si < INDEX_SENTINEL_VALUE as usize);
                    let start = STORAGE_CHUNK_SIZE - self.bytes_left;
                    for (i, b) in s.bytes().enumerate() {
                        self.storage[si][start + i] = b;
                    }
                    self.bytes_left -= s.len();
                    new_key = unsafe {
                        SSOStringRef::new_unchecked(
                            &self.storage[si]
                                .get(start..start + s.len())
                                .unwrap(),
                        ).unwrap()
                    };
                    std::debug_assert!(start < u16::max_value() as usize);
                    istr = InternedStr {
                        index: si as u32,
                        start: start as u16,
                        len: s.len() as u16,
                    };
                } else if s.len() <= STORAGE_CHUNK_SIZE {
                    let si = self.storage.len();
                    // TODO: Not 100% sure if the -1 is needed.
                    // -1 because the new len() for storage will be si + 1
                    // and we still want si + 1 < INDEX_SENTINEL_VALUE.
                    // However, checking that directly might overflow.
                    std::debug_assert!(si < INDEX_SENTINEL_VALUE as usize - 1);
                    self.storage.push([0; STORAGE_CHUNK_SIZE]);
                    for (i, b) in s.bytes().enumerate() {
                        self.storage[si][i] = b;
                    }
                    self.bytes_left = STORAGE_CHUNK_SIZE - s.len();
                    new_key = unsafe {
                        SSOStringRef::new_unchecked(
                            &self.storage[si].get(0..s.len()).unwrap(),
                        )
                        .unwrap()
                    };
                    istr = InternedStr {
                        index: si as u32,
                        start: 0,
                        len: s.len() as u16,
                    };
                } else {
                    let ix = self.huge_strings.len();
                    self.huge_strings.push(s.to_string());
                    new_key = SSOStringRef::new(self.huge_strings[ix].as_str())
                        .unwrap();
                    std::debug_assert!(ix <= u16::max_value() as usize);
                    istr = InternedStr {
                        index: INDEX_SENTINEL_VALUE,
                        start: ix as u16,
                        len: 0,
                    };
                }
                v.insert_hashed_nocheck(hash_value, new_key, istr);
                Ok(istr)
            }
        }
    }
}

// impl Interner {
// }

// #[test]
// fn interning_is_safe() {
//     let xs = vec!["Hello", "World"];
//     let mut interner = Interner::new();
//     let res = vec![];
//     for x in xs.iter() {
//         let istr = interner.insert(x);
//         res.push(istr);
//     }
//     for (i, x) in xs.iter().enumerate() {
//         let x2 = interner.get_str(res[i]);
//         assert!(x2.is_some() && &x2.unwrap() == x);
//     }
// }

// // #[cfg(test)]
// // mod tests {
// //     #[macro_use(quickcheck)]
// //     #[quickcheck]
// //     fn interning_is_safe(xs: Vec<String>) {
// //         let mut res = vec![];
// //         let interner: super::Interner = (|| {
// //             let mut interner = super::Interner::new();
// //             {
// //                 for x in xs.iter() {
// //                     let istr = interner.insert(&x);
// //                     res.push(istr);
// //                 }
// //             }
// //             return interner;
// //         })();
// //         for (i, x) in xs.iter().enumerate() {
// //             let istr = res[i];
// //             let res = interner.get_str_debug(istr);
// //             assert!(res.is_ok());
// //             let res = res.unwrap();
// //             assert!(res == x);
// //         }
// //     }
// // }
