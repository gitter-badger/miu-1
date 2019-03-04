// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use numark::diagram;
use diagram::parse_diagram;
use diagram::to_svg::ToSvg;

fn main() {
    let s1 = "o--\\n";
    let s2 = "   |\n";
    let s3 = "   v\n";
    let s4 = "   *\n";
    let dia = parse_diagram(format!("{}{}{}{}", s1, s2, s3, s4));
    // println!("{:?}", dia);
    for p in dia.paths.iter() {
        println!("{}", p.to_svg());
    }
}
