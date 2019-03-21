
fn main() {
    let p: std::path::PathBuf = ["miuki", "miu-ts-parser"].iter().collect();
    println!("cargo:rustc-link-search=native={}", p.as_path().display());
    println!("cargo:rustc-link-lib=static=miuparser");
}
