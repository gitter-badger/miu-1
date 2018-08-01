use mstring::MString;

#[derive(Debug)]
pub struct Ident(MString);

impl From<&str> for Ident {
    #[inline(always)]
    fn from(s: &str) -> Ident {
        Ident(MString::from(s))
    }
}

#[derive(Debug)]
pub struct TypeDef {
    pub tyname: Ident,
    pub arms: Vec<(Ident, Ident)>,
}

#[derive(Debug)]
pub struct TypeAlias {
    pub tyname: String,
    pub rhs: String,
}


/*

pub Kw: Kw = {
  "rec"      => Kw::Rec,
  "let"      => Kw::Let,
  "in"       => Kw::In,
  "as"       => Kw::As,
  "and"      => Kw::And,
  "where"    => Kw::Where,
  "type"     => Kw::Type,
  "mod"      => Kw::Mod,
  "implicit" => Kw::Implicit,
  "deriving" => Kw::Deriving,
  "forall"   => Kw::Forall,
  "exists"   => Kw::Exists,
  "do"       => Kw::Do,
  "if"       => Kw::If,
  "then"     => Kw::Then,
  "else"     => Kw::Else,
  "match"    => Kw::Match,
  "with"     => Kw::With,
  "import"   => Kw::Import,
  "except"   => Kw::Except,
  "allbut"   => Kw::Allbut,
  "foreign"  => Kw::Foreign,
  "volatile" => Kw::Volatile,
  "atomic"   => Kw::Atomic,
  "alias"    => Kw::Alias,
  "family"   => Kw::Family,
  "map"      => Kw::Map,
  "default"  => Kw::Default,
  "comptime" => Kw::Comptime,
  "tailcall" => Kw::Tailcall,
}


pub Integer: Int = {
DecInt,
HexInt,
OctInt,
BinInt,
}

DecInt: Int = <s:r"(\+|-)?0(_|0)*|[1-9](_|[0-9])*">
    => Int::from_str(&s.replace("_", "")).unwrap();

HexInt: Int = <s:r"(\+|-)?0x[0-9A-Fa-f](_|[0-9A-Fa-f])*">
    => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 16).unwrap();

OctInt: Int = <s:r"(\+|-)?0o([0-7])(_|[0-7])*">
   => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 8).unwrap();

BinInt: Int = <s:r"(\+|-)?0b(0|1)(_|0|1)*">
   => Int::from_str_radix(&s.split_at(2).1.replace("_", ""), 2).unwrap();

*/
