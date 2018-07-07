extern crate miuki;

#[macro_use] extern crate lalrpop_util;

use miuki::test;

lalrpop_mod!(pub calc); // synthesized by LALRPOP

#[test]
fn calc() {
    assert!(calc::TermParser::new().parse("22").is_ok());
    assert!(calc::TermParser::new().parse("(22)").is_ok());
    assert!(calc::TermParser::new().parse("((((22))))").is_ok());
    assert!(calc::TermParser::new().parse("((22)").is_err());
}

fn main() {
    test();
}
