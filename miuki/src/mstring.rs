pub struct MString(String);

impl From<&str> for MString {
    #[inline(always)]
    fn from(s: &str) -> MString {
        MString(String::from(s));
    }
}
