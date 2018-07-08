#![feature(use_extern_macros)]
extern crate istring;
extern crate ramp;

extern crate plex;

pub mod pos;
pub mod span;
pub mod ast;
pub mod lexer;

pub fn test() {
    println!("Test");
}
