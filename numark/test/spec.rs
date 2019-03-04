extern crate numark;

use numark::diagram;

#[cfg(tests)]
mod diagram_spec;

use diagram::to_svg::ToSvg;
use diagram::parse_diagram;

#[test]
fn no_overlapping_paths() {
    let s = "---";
    let diagram = parse_diagram(s.to_string());
    assert!(diagram.num_paths() == 1);
}
#[test]
fn test_diagram_works() {
    // let s1 = " .---. \n";
    // let s2 = " |   | \n";
    // let s3 = " '-+-' \n";
    // let s4 = "   |   \n";
    let s1 = "o--\\n";
    let s2 = "   |\n";
    let s3 = "   v\n";
    let s4 = "   *\n";
    let dia = parse_diagram(format!("{}{}{}{}", s1, s2, s3, s4));
    println!("{:?}", dia);
    for p in dia.paths.iter() {
        println!("{}", p.to_svg());
    }
}
