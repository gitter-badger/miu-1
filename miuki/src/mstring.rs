#[derive(Debug)]
pub struct MString(String);

impl<'a> From<&'a str> for MString {
    #[inline(always)]
    fn from(s: &'a str) -> MString {
        MString(String::from(s))
    }
}
