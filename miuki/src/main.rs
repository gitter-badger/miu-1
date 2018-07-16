#![feature(rust_2018_preview)]

extern crate miuki;
extern crate ramp;

use miuki::test;
use miuki::lexer;
use miuki::parser;

use std::path::{Path};

// lalrpop_mod!(pub calc); // synthesized by LALRPOP

/*
#[test]
fn calc() {
    assert!(calc::TermParser::new().parse("22").is_ok());
    assert!(calc::TermParser::new().parse("(22)").is_ok());
    assert!(calc::TermParser::new().parse("((((22))))").is_ok());
    assert!(calc::TermParser::new().parse("((22)").is_err());
}

#[test]
fn parser() {
    let p = parser::NatParser::new();
    assert!(p.parse("0").is_ok());
    assert!(p.parse("0123").is_err());
    assert!(p.parse("999_999_999_999_999_999_999_999").is_ok());
    assert!(p.parse("0b0_010_010_0").is_ok());
    assert!(p.parse("0x").is_err());
    assert!(p.parse("0o").is_err());
    assert!(p.parse("0b").is_err());
    assert!(p.parse("0xDEADbeef").is_ok());
    assert!(p.parse("0o47_57_42_00").is_ok());
}
*/

fn main() {
    test();
    let p = parser::TypeDefnParser::new();
    let l = lexer::Lexer::new("type Foo =Bar");
    let z = lexer::Lexer::new("type Foo =Bar");
    println!(
        "{:?}",
        z.collect::<Vec<<lexer::Lexer<'_> as Iterator>::Item>>()
    );
    println!("{:?}", p.parse(l));

    let s = std::fs::read_to_string(Path::new("test/generated/10k.miu"))
        .unwrap();
    let l2 = lexer::Lexer::new(&s);
    let p2 = parser::ProgramParser::new();
    match p2.parse(l2) {
        Ok(_) => {
            println!("Done!");
        }
        Err(e) => {
            println!("{:?}", e);
        }
    }
}

/*

pub Nat: Int = {
DecNat,
HexNat,
OctNat,
BinNat,
}

DecNat: Int = <s:r"0(_|0)*|[1-9](_|[0-9])*">
    => Int::from_str(&s.replace("_", "")).unwrap();

HexNat: Int = <s:r"0x[0-9A-Fa-f](_|[0-9A-Fa-f])*">
    => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 16).unwrap();

OctNat: Int = <s:r"0o([0-7])(_|[0-7])*">
   => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 8).unwrap();

BinNat: Int = <s:r"0b(0|1)(_|0|1)*">
   => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 2).unwrap();

*/
