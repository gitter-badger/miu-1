// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod primitives;
mod v2;
mod path;
mod decoration;
mod grid;
pub mod to_svg;

#[allow(dead_code)]
use self::grid::{Grid, find_paths, find_decorations};
use self::path::PathSet;
use self::decoration::DecorationSet;

use regex::Regex;

#[derive(Debug)]
pub struct Diagram {
    grid: Grid,
    paths: PathSet,
    decorations: DecorationSet
}

impl Diagram {
    pub fn num_paths(&self) -> usize {
        self.paths.len()
    }
}

fn equalize_line_lengths(_s: &mut str) {
    ();
}

const HIDE_O: char = '\u{e004}';

// Pixels per character
const SCALE: u32 = 8;

// Multiply Y coordinates by this when generating the final SVG
// result to account for the aspect ratio of text files. This
// MUST be 2.
const ASPECT: u32 = 2;

#[allow(unused_variables)]
fn mut_replace(body: &mut str, re: Regex, subst: String) {
    ();
}

#[allow(unused)]
pub fn parse_diagram(mut ss: String) -> Diagram {
    let s: &mut str = &mut ss;
    equalize_line_lengths(s);

    // let DIAGONAL_ANGLE: f32 = f32::atan(1.0 / ASPECT as f32) * 180.0 / PI;

    // diagramString = diagramString.rp(/([a-zA-Z]{2})o/g, '$1' + HIDE_O);
    // diagramString = diagramString.rp(/o([a-zA-Z]{2})/g, HIDE_O + '$1');
    // diagramString = diagramString.rp(/([a-zA-Z\ue004])o([a-zA-Z\ue004])/g, '$1' + HIDE_O + '$2');
    // TODO: Write regexes.
    // let re1 = Regex::new("o").unwrap();
    // let re2 = Regex::new("o").unwrap();
    // let re3 = Regex::new("o").unwrap();
    // mut_replace(s, re1, format!("$1{:?}", HIDE_O));
    // mut_replace(s, re2, format!("{:?}$1", HIDE_O));
    // mut_replace(s, re3, format!("$1{:?}$2", HIDE_O));

    let mut grid = Grid::from(&*s);
    let mut paths = PathSet::new();
    let mut decorations = DecorationSet::new();

    find_paths(&mut grid, &mut paths);
    find_decorations(&mut grid, &mut paths, &mut decorations);
    Diagram { grid, paths, decorations }
}

#[cfg(tests)]
mod tests {
    use super::decoration::Angle;
    use super::ASPECT;
    use std::f64::consts::PI;
    #[test]
    fn markdeep_consistent_diagonal_angle() {
        assert!(Angle::DIAGONAL.get() == (f64::atan(1.0 / ASPECT as f64) * 180.0 / PI) as u16);
    }
}
