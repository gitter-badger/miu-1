#![feature(use_extern_macros)]
extern crate istring;
extern crate ramp;
extern crate plex;
extern crate regex;
#[macro_use]
extern crate lalrpop_util;

use plex::{lexer, parser};

pub mod pos;
pub mod span;
pub mod ast;
pub mod lexer;
lalrpop_mod!(pub parser);

pub fn test() {
    println!("Test");
}
