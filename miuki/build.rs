extern crate cc;

fn main() {
    cc::Build::new()
        .include("./tree-sitter-miu/src")
        .file("./tree-sitter-miu/src/parser.c")
        .compile("miuparser");
    cc::Build::new()
        .cpp(true)
        .include("./tree-sitter-miu/src")
        .file("./tree-sitter-miu/src/scanner.cc")
        .compile("miuscanner");
}
