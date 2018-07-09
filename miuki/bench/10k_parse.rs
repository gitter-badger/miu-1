#[macro_use]
extern crate criterion;
extern crate miuki;

use std::path::Path;
use miuki::lexer as lexer;
use miuki::parser as parser;

use criterion::Criterion;

fn parse_10k(c: &mut Criterion) {
    let s = std::fs::read_to_string(Path::new("test/gen/10k.miu"))
        .unwrap();
    c.bench_function("parse_10k", move |b| {
        b.iter(|| {
            let l = lexer::Lexer::new(&s);
            let p = parser::ProgramParser::new();
            assert!(p.parse(l).is_ok());
        })
    });
}

criterion_group!(bench, parse_10k);
criterion_main!(bench);
