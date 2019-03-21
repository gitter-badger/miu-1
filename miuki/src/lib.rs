// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(hash_raw_entry)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

extern crate plex;
extern crate ramp;
extern crate regex;

// use plex::{lexer, parser};

pub mod intern;
pub mod ast;
pub mod lexer;
mod mstring;
pub mod pos;
pub mod span;

pub fn test() {
    println!("Test");
}
