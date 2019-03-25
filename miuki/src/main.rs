// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate miuki;
extern crate ramp;
extern crate tree_sitter;

// use miuki::test;

use tree_sitter::{Parser};

use std::io::Read;

fn main() -> std::io::Result<()> {
    let mut parser = Parser::new();
    let language = unsafe { miuki::parser::tree_sitter_miu() };
    parser.set_language(language).unwrap();
    let mut source_code: String = "".to_string();
    println!("{}", std::env::current_dir()?.display());
    std::io::BufReader::new(std::fs::File::open("test.miu")?)
        .read_to_string(&mut source_code)?;
    let tree = parser.parse(source_code, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
    Ok(())
}
