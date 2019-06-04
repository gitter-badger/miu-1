#![allow(warnings)]

use crate::intern::{InternedStr, Interner};
use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::vec::Vec;
use tree_sitter::{Node, Parser, Tree};
use typed_arena::Arena;

pub struct Module<'m, V> {
    imports: Vec<Import<V>>,
    types: Vec<TypeDefinition<'m, V>>,
    values: Vec<ValueDefinition<'m, V>>,
    tree: &'m Tree,
    invariants: Invariants,
    expr_pool: &'m Arena<Expr<'m, V>>,
    node_pool: &'m Arena<Node<'m>>,
    pattern_pool: &'m Arena<Pattern<'m, V>>,
    interner: &'m Interner<'m>,
}

/// A "wip" struct to create a Module.
///
/// 's - lifetime of the source string, which is the longest
/// 't - lifetime of the tree-sitter generated tree, which may be shorter
/// 'm - lifetime of the Module and internal references which is the shortest
pub struct ModuleWip<'m, 't, 's, V> {
    src: &'s str,
    tree: &'t Tree,
    errs: Errors<'t>,
    val_sigs: Vec<(InternedStr<'m>, Type<'m>)>,
    val_defs: Vec<LetBinding<'m, &'m str>>,
    expr_pool: &'m Arena<Expr<'m, V>>,
    pattern_pool: &'m Arena<Pattern<'m, V>>,
    node_pool: &'m Arena<Node<'m>>,
    interner: &'m Interner<'m>,
}

pub struct Pools<'m, V> {
    exprs: Arena<Expr<'m, V>>,
    patterns: Arena<Pattern<'m, V>>,
    nodes: Arena<Node<'m>>,
    interner: Interner<'m>,
}

macro_rules! bad_cst {
    ( $i:ident, $x:expr, $e:expr ) => {
        ($e).push(BadCstError::$i($x))
    };
}

macro_rules! bad_cst_child {
    ( $x:expr, $e:expr ) => {
        ($e).push(BadCstError::is_unexpected_child($x)).map(|x| x)
    };
}

// This seems like a misnomer :(
macro_rules! bad_cst_parent {
    ( $x:expr, $e:expr ) => {
        ($e).push(BadCstError::has_unexpected_children($x))
            .map(|x| x)
    };
}

impl<'m, 't: 'm, 's: 't, V> ModuleWip<'m, 't, 's, V> {
    fn get_istr(&mut self, n: Node<'t>) -> InternedStr<'m> {
        let s = self.get_str(n);
        self.interner.insert(s)
    }

    fn get_str(&mut self, n: Node<'t>) -> &'m str {
        let s = self.src.as_bytes();
        n.utf8_text(s).unwrap()
    }

    fn process_type(&mut self, t: Node<'t>) -> Result<Type<'m>, ()> {
        unimplemented!()
    }

    // TODO: We should have our own standards for parsing,
    // and not blindly use Rust's defaults.
    fn parse_integer(s: &str) -> Option<i64> {
        s.parse::<i64>().ok()
    }

    // TODO: We should have our own standards for parsing strings,
    // and not blindly use Rust's defaults.
    fn parse_string(s: &str) -> Option<String> {
        s.parse::<std::string::String>().ok()
    }

    fn process_literal_expression(
        &mut self,
        n: Node<'t>,
    ) -> Result<Expr<'m, &'m str>, ()> {
        use ExprCases::*;
        use Literal::*;
        macro_rules! ok_expr {
            ($x:expr) => {
                Ok(Expr {
                    get: Spanned {
                        get: Lit($x),
                        node: self.node_pool.alloc(n),
                    },
                })
            };
        };
        match n.child(0) {
            Some(n) => match n.kind() {
                "unit" => ok_expr!(Unit),
                "integer" => match Self::parse_integer(self.get_str(n)) {
                    Some(i) => ok_expr!(Int64(i)),
                    None => unimplemented!(),
                },
                "string" => match Self::parse_string(self.get_str(n)) {
                    Some(s) => {
                        let istr = self.interner.insert(&s);
                        let sref = self.interner.get_str_unchecked(istr);
                        ok_expr!(Literal::String(sref))
                    }
                    None => self.errs.push(BadCstError {
                        reason: ErrorDetails::UserError(
                            UserError::IllFormedString,
                        ),
                        bad_node: n,
                    }),
                },
                "True" => ok_expr!(Bool(true)),
                "False" => ok_expr!(Bool(false)),

                _ => bad_cst_child!(n, self.errs),
            },
            _ => bad_cst_parent!(n, self.errs),
        }
    }

    fn process_literal_pattern(
        &mut self,
        n: Node<'t>,
    ) -> Result<Pattern<'m, &'m str>, ()> {
        match n.child(0) {
            Some(n) if n.kind() == "literal_expression" => {
                self.process_literal_expression(n).map(|e| Pattern {
                    get: e.get.map(Box::new(|ee| match ee {
                        ExprCases::Lit(l) => PatternCases::Lit(l),
                        _ => unreachable!(),
                    })),
                })
            }
            _ => bad_cst_parent!(n, self.errs),
        }
    }

    fn process_eliminator_pattern(
        &mut self,
        n: Node<'t>,
    ) -> Result<Pattern<'m, &'m str>, ()> {
        match n.child(0) {
            Some(n) if n.kind() == "path" => {
                for n in n.children().skip(1) {}
                Err(())
            }
            _ => bad_cst_parent!(n, self.errs),
        }
    }

    fn try_convert_pattern_into_binding(
        &mut self,
        pat: Pattern<'m, &'m str>,
    ) -> Result<Binding<'m, &'m str>, ()> {
        unimplemented!()
    }

    fn process_record_pattern(
        &mut self,
        n: Node<'t>,
    ) -> Result<Pattern<'m, &'m str>, ()> {
        unimplemented!()
    }

    fn process_pattern(
        &mut self,
        n: Node<'t>,
    ) -> Result<Pattern<'m, &'m str>, ()> {
        match n.child(0) {
            Some(n) => match n.kind() {
                "literal_pattern" => self.process_literal_pattern(n),
                "(" => match n.next_sibling() {
                    Some(n) if n.kind() == "pattern" => self.process_pattern(n),
                    _ => bad_cst_parent!(n, self.errs),
                },
                "eliminator_pattern" => self.process_eliminator_pattern(n),
                "record_pattern" => self.process_record_pattern(n),
                _ => bad_cst_child!(n, self.errs),
            },
            _ => bad_cst_parent!(n, self.errs),
        }
    }

    fn process_let_binding(&mut self, n: Node<'t>) -> Result<(), ()> {
        let mut is_recursive = false;
        let mut i = 1;
        match n.child(i) {
            Some(c) => match c.kind() {
                "rec" => {
                    is_recursive = true;
                    i += 1;
                }
                "binding" => {}
                _ => {
                    return bad_cst_child!(c, self.errs);
                }
            },
            None => {
                return bad_cst_parent!(n, self.errs);
            }
        }
        let bind = match n.child(i) {
            Some(n) if i == 2 || n.kind() == "binding" => match n.child(0) {
                Some(n) if n.kind() == "pattern" => {
                    let p = self.process_pattern(n);
                    match p {
                        Ok(p) => match self.try_convert_pattern_into_binding(p)
                        {
                            // match p.try_into_binding(&mut self.errs) {
                            Ok(bind) => bind,
                            _ => {
                                // TODO: record an error here...
                                return unimplemented!();
                            }
                        },
                        _ => {
                            // TODO: record an error here...
                            return unimplemented!();
                        }
                    }
                }
                _ => {
                    return bad_cst_parent!(n, self.errs);
                }
            },
            _ => {
                return bad_cst_parent!(n, self.errs);
            }
        };
        unimplemented!()
    }

    fn process_top_value_signature(&mut self, n: Node<'t>) -> Result<(), ()> {
        match (n.child(1), n.child(3)) {
            (Some(i), Some(t))
                if i.kind() == "identifier" && t.kind() == "type" =>
            {
                if let Ok(ty) = self.process_type(t) {
                    let istr = self.get_istr(n);
                    self.val_sigs.push((istr, ty));
                    Ok(())
                } else {
                    unimplemented!()
                }
            }
            _ => bad_cst_parent!(n, self.errs),
        }
    }
}

impl<'m, V: Debug> Debug for Module<'m, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Module {{\n  imports: {:?},\n  types: {:?},\n  values: {:?} }}",
            self.imports, self.types, self.values
        )
    }
}

#[derive(Debug)]
struct Import<V> {
    get: V,
}

#[derive(Debug)]
struct Invariants {
    get: (),
}

#[derive(Debug)]
struct TypeDefinition<'m, V> {
    kind_signature: Option<Type<'m>>,
    binding: TypeBinding<V>,
}

#[derive(Debug)]
enum TypeBinding<V> {
    Single(PlainTypeBinding<V>),
    // Invariant: The vector must be non-empty.
    Recursive(Vec<PlainTypeBinding<V>>),
}

#[derive(Debug)]
struct PlainTypeBinding<V> {
    lhs: Vec<Variable<V>>,
    rhs: (),
    is_alias: Option<bool>,
}

#[derive(Debug)]
struct Typed<'m, A> {
    ann: Type<'m>,
    item: A,
}

#[derive(Debug)]
struct Type<'m> {
    get: &'m (),
}

#[derive(Debug)]
struct Spanned<'m, A> {
    get: A,
    node: &'m Node<'m>,
}

impl<'m, A> Spanned<'m, A> {
    fn map<B>(self, f: Box<dyn FnOnce(A) -> B>) -> Spanned<'m, B> {
        Spanned {
            get: f(self.get),
            node: self.node,
        }
    }
}

#[derive(Debug)]
struct ValueDefinition<'m, V> {
    type_signature: Option<Type<'m>>,
    binding: LetBinding<'m, V>,
}

#[derive(Debug)]
struct ExprRef<'m, V> {
    get: &'m mut Expr<'m, V>,
    _a: PhantomData<V>,
}

#[derive(Debug)]
struct PatRef<'m, V> {
    get: &'m mut Pattern<'m, V>,
    _a: PhantomData<V>,
}

#[derive(Debug)]
struct Expr<'m, V> {
    get: Spanned<'m, ExprCases<'m, V>>,
}

#[derive(Debug)]
enum ExprCases<'m, V> {
    Lit(Literal<'m>),
    Var(Variable<V>),
    Let(LetBinding<'m, V>),
    Lam(Lambda<'m, V>),
    App(Application<'m, V>),
    // If statements are converted to case statements right away.
    Case(CaseExpr<'m, V>),
    Rec(Record<'m, V>),
    Proj {
        item: ExprRef<'m, V>,
        label: Variable<V>,
    },
}

/// For now, we don't distinguish between Miu values and Rust values.
#[derive(Debug)]
enum Literal<'m> {
    Unit,
    Int64(i64),
    Bool(bool),
    String(&'m str),
}

#[derive(Debug)]
struct Variable<V> {
    var: V,
}

#[derive(Debug)]
enum LetBinding<'m, V> {
    Single(PlainLetBinding<'m, V>),
    // Invariant: The vector must be non-empty.
    Recursive(Vec<PlainLetBinding<'m, V>>),
}

/// We don't support pattern matching in bindings for now.
/// Use explicit case instead.
#[derive(Debug)]
struct PlainLetBinding<'m, V> {
    // Invariant: This vector is non-empty.
    lhs: Binding<'m, V>,
    rhs: ExprRef<'m, V>,
    body: ExprRef<'m, V>,
}

#[derive(Debug)]
struct Binding<'m, V> {
    // Invariant: The list is non-empty and the first item is a variable.
    get: Vec<Pattern<'m, V>>,
}

#[derive(Debug)]
struct Lambda<'m, V> {
    lhs: Vec<Typed<'m, Variable<V>>>,
    body: ExprRef<'m, V>,
}

#[derive(Debug)]
struct Application<'m, V> {
    // Invariant: This vector has two or more elements.
    app: Vec<ExprRef<'m, V>>,
}

#[derive(Debug)]
struct CaseExpr<'m, V> {
    head: ExprRef<'m, V>,
    arms: Vec<(Pattern<'m, V>, ExprRef<'m, V>)>,
}

#[derive(Debug)]
struct Pattern<'m, V> {
    get: Spanned<'m, PatternCases<'m, V>>,
}

impl<'m, V> Pattern<'m, V> {
    fn try_into_binding<'t: 'm>(
        self,
        errs: &mut Errors<'t>,
    ) -> Result<Binding<'m, &'m str>, ()> {
        unimplemented!()
    }
}

#[derive(Debug)]
enum PatternCases<'m, V> {
    Lit(Literal<'m>),
    // This also accounts for wildcards.
    Var(Variable<V>),
    Elim(Eliminator<'m, V>),
}

#[derive(Debug)]
struct Eliminator<'m, V> {
    head: DataCon<V>,
    body: Vec<PatRef<'m, V>>,
}

// This representation needs to change...
#[derive(Debug)]
struct DataCon<V> {
    get: Variable<V>,
}

#[derive(Debug)]
struct Record<'m, V> {
    fields: Vec<RecordField<'m, V>>,
}

#[derive(Debug)]
enum RecordField<'m, V> {
    Punny(Variable<V>),
    Plain {
        label: Variable<V>,
        value: ExprRef<'m, V>,
    },
}

#[derive(Debug)]
pub struct BadCstError<'a> {
    reason: ErrorDetails,
    bad_node: Node<'a>,
}

#[derive(Debug)]
pub enum ErrorDetails {
    InternalError(InternalError),
    UserError(UserError),
}

#[derive(Debug)]
pub enum InternalError {
    IsUnexpectedChild,
    HasUnexpectedChildren,
}

#[derive(Debug)]
pub enum UserError {
    IllFormedInteger,
    IllFormedString,
}

use self::ErrorDetails::*;
use self::InternalError::*;
use self::UserError::*;

impl<'a> BadCstError<'a> {
    fn is_unexpected_child(n: Node<'a>) -> BadCstError<'a> {
        BadCstError {
            reason: InternalError(IsUnexpectedChild),
            bad_node: n,
        }
    }
    fn has_unexpected_children(n: Node<'a>) -> BadCstError<'a> {
        BadCstError {
            reason: InternalError(HasUnexpectedChildren),
            bad_node: n,
        }
    }
}

fn non_comment_children<'a>(n: &Node<'a>) -> impl Iterator<Item = Node<'a>> {
    n.children().filter(|n| n.kind() != "comment")
}

#[derive(Debug)]
pub struct Errors<'a>(Vec<BadCstError<'a>>);

impl<'a> Errors<'a> {
    fn new(v: Vec<BadCstError<'a>>) -> Errors<'a> {
        Errors(v)
    }
    fn push<T>(&mut self, err: BadCstError<'a>) -> Result<T, ()> {
        self.0.push(err);
        Err(())
    }
}

impl<'m> Module<'m, &'m str> {
    pub fn from_cst<'t: 'm, 's: 't>(
        src: &'s str,
        tree: &'t Tree,
        p: &'t mut Pools<&'m str>,
    ) -> Result<Module<'m, &'m str>, Errors<'t>> {
        let n = tree.root_node();
        if n.kind() != "source_file" {
            return Err(Errors::new(vec![BadCstError::is_unexpected_child(n)]));
        }
        // let mut p = Pools {
        //     exprs: Arena::with_capacity(2048),
        //     patterns: Arena::with_capacity(512),
        //     nodes: Arena::with_capacity(512),
        //     interner: Interner::empty(),
        // };
        let mut m = ModuleWip::<&str> {
            src: src,
            tree: tree,
            errs: Errors::new(vec![]),
            val_sigs: Vec::with_capacity(32),
            val_defs: Vec::with_capacity(32),
            expr_pool: &p.exprs,
            pattern_pool: &p.patterns,
            node_pool: &p.nodes,
            interner: &mut p.interner,
        };
        for n in non_comment_children(&n) {
            match n.kind() {
                "definition" => {
                    for n in non_comment_children(&n) {
                        match n.kind() {
                            "top_value_signature" => {
                                m.process_top_value_signature(n);
                            }
                            "top_value_definition" => {}
                            "top_type_signature" => {}
                            "top_type_definition" => {}
                            _ => {
                                let _: Result<(), _> =
                                    bad_cst_child!(n, m.errs);
                            }
                        }
                    }
                }
                _ => {
                    let ret =
                        Errors::new(vec![BadCstError::is_unexpected_child(n)]);
                    println!("{:?}", ret);
                    return Err(ret);
                }
            }
        }
        return Err(Errors::new(vec![]));
    }
}

#[test]
fn test_cst() {
    let mut parser = Parser::new();
    let lang = unsafe { crate::parser::tree_sitter_miu() };
    parser.set_language(lang).unwrap();
    let src = "let x = 10\n-- Comment".to_string();
    let tree = parser.parse(&src, None).unwrap();
    Module::from_cst(&src, &tree);
}
