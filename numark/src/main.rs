// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use numark::diagram;
use diagram::parse_diagram;
use diagram::to_svg::ToSvg;

fn main() {
    let s1 = r"o--.";
    let s2 = r"   |";
    let s3 = r"   v";
    let s4 = r"   *";
    let dia = parse_diagram(&vec![s1, s2, s3, s4].as_slice());
    println!("{}", dia.to_svg());
    // println!("{:?}", dia);
    // for p in dia.paths.iter() {
    //     println!("{}", p.to_svg());
    // }
    // println!("{}", dia.decorations.to_svg());
    // println!("{:?}", dia.decorations);
    // for d in dia.decorations.iter() {
    //     println!("{}", (*d).to_svg());
    // }
}
