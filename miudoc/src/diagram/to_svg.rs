use super::path::Path;
use svg::node::element::Path as SvgPath;

trait ToSvg {
    fn to_svg(&self) -> String;
}

impl ToSvg for Path {
    fn to_svg(&self) -> String {
        let mut p = SvgPath::new();
        let mut d = Data::new();
        d.move_to(p.a);
        p.set("fill", "none");
        if self.is_curved() {
            d.cubic_curve_to(p.c);
            d.cubic_curve_to(p.d);
        } else {
            d.line_to(p.b);
        }
        d.close();
        if self.is_dashed() {
            p.set("stroke-dasharray", "3,6");
        }
        "Hello".to_string()
    }
}
