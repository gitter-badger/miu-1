#![feature(use_extern_macros)]
extern crate plex;
extern crate ramp;
extern crate regex;
#[macro_use]
extern crate lalrpop_util;

use plex::{lexer, parser};

pub mod ast;
pub mod lexer;
mod mstring;
pub mod pos;
pub mod span;
lalrpop_mod!(pub parser);

pub fn test() {
    println!("Test");
}
