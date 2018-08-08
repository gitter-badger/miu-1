extern crate lalrpop;

fn main() {
    let _tmp = lalrpop::Configuration::new()
        .always_use_colors()
        .process_current_dir();
}
