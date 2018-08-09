mod primitives;
mod grid;

#[allow(dead_code)]
use self::primitives::*;
use self::grid::*;

use regex::Regex;

use std::f32::consts::PI;

type Diagram = ();

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

const EPSILON: f32 = 1E-6;

#[allow(unused_variables)]
fn mut_replace(body: &mut str, re: Regex, subst: String) {
    ();
}

#[allow(unused_variables)]
fn parse_diagram(mut ss: String) -> Diagram {
    let s: &mut str = &mut ss;
    equalize_line_lengths(s);

    let DIAGONAL_ANGLE: f32 = f32::atan(1.0 / ASPECT as f32) * 180.0 / PI;

    // diagramString = diagramString.rp(/([a-zA-Z]{2})o/g, '$1' + HIDE_O);
    // diagramString = diagramString.rp(/o([a-zA-Z]{2})/g, HIDE_O + '$1');
    // diagramString = diagramString.rp(/([a-zA-Z\ue004])o([a-zA-Z\ue004])/g, '$1' + HIDE_O + '$2');
    let re1 = Regex::new("o").unwrap();
    let re2 = Regex::new("o").unwrap();
    let re3 = Regex::new("o").unwrap();
    mut_replace(s, re1, format!("$1{:?}", HIDE_O));
    mut_replace(s, re2, format!("{:?}$1", HIDE_O));
    mut_replace(s, re3, format!("$1{:?}$2", HIDE_O));

    let decoration_chars: Vec<_> = ARROW_HEAD_CHARS
        .iter()
        .chain(&POINT_CHARS)
        .chain(&JUMP_CHARS)
        .chain(&GRAY_CHARS)
        .chain(&TRI_CHARS)
        .collect();

    ()
}
