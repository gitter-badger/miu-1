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
