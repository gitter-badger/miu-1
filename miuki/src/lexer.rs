use ramp::Int;
use istring::IString;
use span::{ByteOffset, Span, TokSpan};
use plex::lexer;

#[derive(Clone, Copy, Debug)]
pub enum Kw {
    Rec,
    Let,
    In,
    As,
    And,
    Where,
    Type,
    Mod,
    Implicit,
    Deriving,
    Forall,
    Exists,
    Do,
    If,
    Then,
    Else,
    Match,
    With,
    Import,
    Except,
    Allbut,
    Foreign,
    Atomic,
    /// Contextual keywords
    Alias,
    Family,
    Map,
    Default,
    /// Reserved words for the future
    Comptime,
    Tailcall,
    Volatile,
}

#[derive(Clone, Copy, Debug)]
pub enum SymKw {
    RArrow,
    LArrow,
    Lollipop,
    Pipe,
    Backslash,
    Dot,
    DotDot,
    Colon,
    Semicolon,
    Equals,
    Qmark,
    QmarkX2,
    LParen,
    RParen,
    DollarLParen,
    LSquare,
    RSquare,
    DollarLSquare,
    LSquareGt,
    LtRSquare,
    LSquareLt,
    GtRSquare,
    LSquarePipe,
    PipeRSquare,
    LBrace,
    RBrace,
    DollarLBrace,
    LBraceGt,
    LtRBrace,
    LBraceLt,
    GtRBrace,
    RArrowRBrace,
    LollipopRBrace,
    // Reserved for future
    Grave,
}

#[derive(Debug)]
pub enum CtxKw {
    Alias,
    Family,
    Map,
    Default,
}

#[derive(Debug)]
pub enum CtxSymKw {
    EqualsX2,
    Implies,
}

#[derive(Debug)]
pub enum Token {
    Whitespace,
    Newline,
    Identifier(String),
    Hole,

    Kw(Kw),
    SymKw(SymKw),
    CtxKw(CtxKw),
    CtxSymKw(CtxSymKw),

    // TODO: Fix the prefix stuff.
    PrefixOp,
    InfixOp(Precedence, Fixity),
    FatArrow(IString),

    IntLit(Int),
    CharLit(char),
    StringLit(IString),

    Quoted(Quoter),

    Comment(CommentStyle),
    Pragma(CommentStyle),
    Doc(CommentStyle, DocAttach),

    EOF,
}

#[derive(Debug, Copy, Clone)]
pub enum Fixity {
    InfixL,
    InfixN,
    InfixR,
}

#[derive(Debug, Copy, Clone)]
pub enum Precedence {
    P0,
    P1,
    P2,
    P3,
    P4,
    P5,
    P6,
    P7,
    P8,
    P9,
    P10,
    P11,
    P12,
    P13,
    P14,
}

#[derive(Debug)]
pub struct Quoter(IString);

#[derive(Debug, Copy, Clone)]
pub enum DocAttach {
    Prev,
    Next,
}

#[derive(Debug, Copy, Clone)]
pub enum CommentStyle {
    Line,
    Block,
}

pub struct MiuFile {
    text: String,
    // newlines: Vec<ByteOffset>,
    pub op_info: Vec<(IString, Precedence, Fixity)>,
    pub shebang: Option<TokSpan>,
}

impl MiuFile {
    pub fn view<'a>(&'a self, s: TokSpan) -> &'a str {
        &self.text[s.start as usize..s.end as usize]
    }
}

#[derive(Debug)]
pub struct Located<P, T> {
    pub span: Span<P>,
    pub val: T,
}
pub type LocatedToken = Located<ByteOffset, Token>;
pub type LocatedError = Located<ByteOffset, LexError>;

#[derive(Debug)]
pub enum LexError {
    EmptyCharLiteral,
}

lexer! {
    fn next_token(tok: 'a) -> Token;

    // r#"[ \t\r\n]+"# => (Token::Whitespace, text),
    // // "C-style" comments (/* .. */) - can't contain "*/"
    // r#"/[*](~(.*[*]/.*))[*]/"# => (Token::Comment, text),
    // // "C++-style" comments (// ...)
    // r#"//[^\n]*"# => (Token::Comment, text),

    // r#"print"# => (Token::Print, text),

    // r#"[0-9]+"# => {
    //     (if let Ok(i) = text.parse() {
    //         Token::Integer(i)
    //     } else {
    //         panic!("integer {} is out of range", text)
    //     }, text)
    // }

    // r"[a-zA-Z_][a-zA-Z0-9_]*" => (Token::Ident(text.to_owned()), text),

    r"[ \t\r\n]+" => (Token::Whitespace),
    // r"\n|\r\n" => (Token::Newline),
    r"type" => (Token::Kw(Kw::Type)),
    r"alias" => (Token::CtxKw(CtxKw::Alias)),
    r"=" => (Token::SymKw(SymKw::Equals)),
    r"\|" => (Token::SymKw(SymKw::Pipe)),
    // r"\(" => (Token::LParen),
    // r"\)" => (Token::RParen),
    r"[A-Za-z0-9]+" => (Token::Identifier(tok.to_string())),
    // r#"\+"# => (Token::Plus),
    // r#"-"# => (Token::Minus),
    // r#"\*"# => (Token::Star, text),
    // r#"/"# => (Token::Slash, text),
    // r#";"# => (Token::Semi, text),
}

pub struct Lexer<'a> {
    rem: &'a str,
    idx: ByteOffset,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer { rem: s, idx: 0 }
    }
}

impl<'a> Lexer<'a> {
    #[inline(always)]
    fn process(&mut self, rem: &'a str) -> u32 {
        let old_idx = self.idx;
        self.idx += (rem.as_ptr() as usize - self.rem.as_ptr() as usize) as u32;
        self.rem = rem;
        old_idx
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (ByteOffset, Token, ByteOffset);

    fn next(&mut self) -> Option<Self::Item> {
        // Manually inlining the function makes the parser go ~1.5% faster.
        match next_token(self.rem) {
            Some((Token::Whitespace, new_rem)) => {
                let _ = self.process(new_rem);
                match next_token(self.rem) {
                    Some((Token::Whitespace, _)) => unreachable!(),
                    Some((tok, new_rem)) => {
                        let old_idx = self.process(new_rem);
                        Some((old_idx, tok, self.idx))
                    }
                    None => None,
                }
            }
            Some((tok, new_rem)) => {
                let old_idx = self.process(new_rem);
                Some((old_idx, tok, self.idx))
            }
            None => None,
        }
    }
}

// pub struct Lexer<'input> {
//     input: &'input str,
//     ahead: Option<char>,
//     start_idx: ByteOffset,
// }

// impl<'input> Lexer<'input> {
//     pub fn new(s: &'input str) -> Tokenizer<'input> {
//     }
// }
