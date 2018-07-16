#[macro_use]
extern crate criterion;
extern crate miuki;

use miuki::lexer;
use miuki::parser;

use criterion::Benchmark;
use criterion::Criterion;
use criterion::Throughput;

use std::path::Path;

fn parse_10k(_: &mut Criterion) {
    let s = std::fs::read_to_string(Path::new("bench/samples/10k.miu")).unwrap();
    Criterion::default().sample_size(250).bench(
        "throughput",
        Benchmark::new("parse_10k", move |b| {
            b.iter(|| {
                let l = lexer::Lexer::new(&s);
                let p = parser::ProgramParser::new();
                assert!(p.parse(l).is_ok());
            })
        }).throughput(
            Throughput::Elements(10000),
        ),
    );
}

criterion_group!(bench, parse_10k);
criterion_main!(bench);
