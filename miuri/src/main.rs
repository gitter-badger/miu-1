// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use pretty::{BoxDoc, Doc};
use tree_sitter::{Node, Parser};

use std::io::Read;

fn node_to_doc<'a>(n: &Node, src: &'a str) -> Doc<'a, BoxDoc<'a, ()>, ()> {
    // const width: usize = 80;
    if n.child_count() == 0 {
        Doc::text("(")
            .append(Doc::text(
                if n.kind() == "(" || n.kind() == ")" {
                    format!("\"{}\"", n.kind())
                } else {
                    n.kind().to_string()
                }
            ))
            .append({
                let s = std::str::from_utf8(
                    src.as_bytes().get(n.start_byte()..n.end_byte()).unwrap()
                ).unwrap();
                if s == n.kind() || s == "" {
                    Doc::nil()
                } else {
                    Doc::space().append(
                        Doc::text(
                            format!("\"{}\"", s)
                        )
                    )
                }
            })
            .append(")")
            .group()
    } else {
        Doc::text("(")
            .append(Doc::text(n.kind()))
            .append(Doc::space())
            .append(
                Doc::intersperse(n.children().map(|c| {
                    node_to_doc(&c, src).nest(2)
                }), Doc::space()).group()
            )
            .append(Doc::text(")"))
            .group()
    }
}

fn node_to_string(n: &Node, src: &str) -> String {
    format!("{}", node_to_doc(n, src).pretty(80))
}

fn main() -> std::io::Result<()> {
    let mut parser = Parser::new();
    let language = unsafe { miuri::parser::tree_sitter_miu() };
    parser.set_language(language).unwrap();
    let mut source_code: String = "".to_string();
    println!("{}", std::env::current_dir()?.display());
    std::io::BufReader::new(std::fs::File::open("tree-sitter-miu/test2.miu")?)
        .read_to_string(&mut source_code)?;
    let tree = parser.parse(&source_code, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
    println!("{}", node_to_string(&tree.root_node(), &source_code));
    Ok(())
}
