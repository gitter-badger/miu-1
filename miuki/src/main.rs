// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate miuki;
extern crate ramp;
extern crate tree_sitter;

// use miuki::test;

use tree_sitter::{Parser, Node};

use std::io::Read;

fn node_to_string(n: &Node, src: &str) -> String {
    if n.child_count() == 0 {
        format!("({} {:?})", n.kind(),
                std::str::from_utf8(
                    src.as_bytes().get(n.start_byte() .. n.end_byte()).unwrap()
                ).unwrap())
    } else {
        let mut s = format!("({}\n", n.kind());
        for c in n.children() {
            let inner = node_to_string(&c, src);
            for l in inner.lines() {
                s.push_str("  ");
                s.push_str(l);
                s.push('\n');
            }
        }
        s.push_str(")");
        s
    }
}

fn main() -> std::io::Result<()> {
    let mut parser = Parser::new();
    let language = unsafe { miuki::parser::tree_sitter_miu() };
    parser.set_language(language).unwrap();
    let mut source_code: String = "".to_string();
    println!("{}", std::env::current_dir()?.display());
    std::io::BufReader::new(std::fs::File::open("tree-sitter-miu/test.miu")?)
        .read_to_string(&mut source_code)?;
    let tree = parser.parse(&source_code, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
    println!("{}", node_to_string(&tree.root_node(), &source_code));
    Ok(())
}
