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
    expr_pool: Arena<Expr<'m, V>>,
    node_pool: Arena<Node<'m>>,
    pattern_pool: Arena<Pattern<'m, V>>,
    interner: Interner<'m>,
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
    expr_pool: Arena<Expr<'m, V>>,
    pattern_pool: Arena<Pattern<'m, V>>,
    node_pool: Arena<Node<'m>>,
    interner: Interner<'m>,
}

macro_rules! is_bad {
    ( $x:expr, $e:expr ) => {
        return ($e).push(BadCstError::is_unexpected_child($x));
    };
}

macro_rules! is_bad_err {
    ( $x:expr, $e:expr ) => {{
        ($e).push(BadCstError::is_unexpected_child($x));
        return Err(());
    }};
}

macro_rules! has_bad {
    ( $x:expr, $e:expr ) => {
        return ($e).push(BadCstError::has_unexpected_children($x));
    };
}

macro_rules! has_bad_err {
    ( $x:expr, $e:expr ) => {{
        ($e).push(BadCstError::has_unexpected_children($x));
        return Err(());
    }};
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

    fn process_literal_expression(
        &mut self,
        n: Node<'t>,
    ) -> Result<Expr<'m, &'m str>, ()> {
        use ExprCases::*;
        use Literal::*;
        match n.child(0) {
            Some(n) => {
                match n.kind() {
                    "unit" => {
                        let n_ref = unsafe { std::mem::transmute(self.node_pool.alloc(n)) };
                        Ok(Expr{get: Spanned{get: Lit(Unit), node: n_ref}})
                    }
                    "integer" => {
                        // TODO: We should have our own standards for parsing,
                        // not use Rust's defaults.
                        let i = self.get_str(n).parse::<i64>();
                        match i {
                            Ok(i) => {
                                let n_ref = unsafe { std::mem::transmute(self.node_pool.alloc(n)) };
                                Ok(Expr{get: Spanned{get: Lit(Int64(i)), node: n_ref}})
                            }
                            Err(e) => unimplemented!(),
                        }
                    },
                    "string" => {
                        let s = self.get_str(n).parse::<std::string::String>();
                        match s {
                            Ok(s) => {
                                let istr = self.interner.insert(&s);
                                let s_ref = unsafe { std::mem::transmute(self.interner.get_str_unchecked(istr)) };
                                let n_ref = unsafe { std::mem::transmute(self.node_pool.alloc(n)) };
                                Ok(Expr{get: Spanned{get: Lit(Literal::String(s_ref)), node: n_ref}})
                            }
                            Err(e) => unimplemented!(),
                        }
                    },
                    "True" => {
                        let n_ref = unsafe { std::mem::transmute(self.node_pool.alloc(n)) };
                        Ok(Expr{get: Spanned{get: Lit(Bool(true)), node: n_ref}})
                    },
                    "False" => {
                        let n_ref = unsafe { std::mem::transmute(self.node_pool.alloc(n)) };
                        Ok(Expr{get: Spanned{get: Lit(Bool(false)), node: n_ref}})
                    },
                    _ => is_bad_err!(n, self.errs),
                }
            }
            _ => has_bad_err!(n, self.errs),
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
            _ => has_bad_err!(n, self.errs),
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
            _ => has_bad_err!(n, self.errs),
        }
    }

    fn try_convert_pattern_into_binding(
        &mut self,
        pat: Pattern<'m, &'m str>,
    ) -> Result<Binding<'m, &'m str>, ()> {
        panic!()
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
                    _ => has_bad_err!(n, self.errs),
                },
                "eliminator_pattern" => self.process_eliminator_pattern(n),
                "record_pattern" => self.process_record_pattern(n),
                _ => is_bad_err!(n, self.errs),
            },
            _ => has_bad_err!(n, self.errs),
        }
    }

    fn process_let_binding(&mut self, n: Node<'t>) {
        let mut is_recursive = false;
        let mut i = 1;
        match n.child(i) {
            Some(c) => match c.kind() {
                "rec" => {
                    is_recursive = true;
                    i += 1;
                }
                "binding" => {}
                _ => is_bad!(c, self.errs),
            },
            None => has_bad!(n, self.errs),
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
                                return;
                            }
                        },
                        _ => {
                            // TODO: record an error here...
                            return;
                        }
                    }
                }
                _ => has_bad!(n, self.errs),
            },
            _ => has_bad!(n, self.errs),
        };
    }

    fn process_top_value_signature(&mut self, n: Node<'t>) {
        match (n.child(1), n.child(3)) {
            (Some(i), Some(t))
                if i.kind() == "identifier" && t.kind() == "type" =>
            {
                if let Ok(ty) = self.process_type(t) {
                    let istr = self.get_istr(n);
                    self.val_sigs.push((istr, ty));
                }
            }
            _ => has_bad!(n, self.errs),
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
pub enum BadCstError<'a> {
    InternalError(InternalErrorReason, Node<'a>),
    UserError(UserErrorReason, Node<'a>),
}

impl<'a> BadCstError<'a> {
    fn is_unexpected_child(n: Node<'a>) -> BadCstError<'a> {
        BadCstError::InternalError(InternalErrorReason::IsUnexpectedChild, n)
    }
    fn has_unexpected_children(n: Node<'a>) -> BadCstError<'a> {
        BadCstError::InternalError(
            InternalErrorReason::HasUnexpectedChildren,
            n,
        )
    }
}

#[derive(Debug)]
pub enum InternalErrorReason {
    IsUnexpectedChild,
    HasUnexpectedChildren,
}

#[derive(Debug)]
pub enum UserErrorReason {
    IllFormedInteger,
}

fn non_comment_children<'a>(n: &Node<'a>) -> impl Iterator<Item = Node<'a>> {
    n.children().filter(|n| n.kind() != "comment")
}

type Errors<'a> = Vec<BadCstError<'a>>;

impl<'m> Module<'m, &'m str> {
    pub fn from_cst<'t: 'm, 's: 't>(
        src: &'s str,
        tree: &'t Tree,
    ) -> Result<Module<'m, &'m str>, Errors<'t>> {
        let n = tree.root_node();
        if n.kind() != "source_file" {
            return Err(vec![BadCstError::is_unexpected_child(n)]);
        }
        let mut m = ModuleWip::<&str> {
            src: src,
            tree: tree,
            errs: vec![],
            // TODO: Think about these numbers more carefully?
            val_sigs: Vec::with_capacity(32),
            val_defs: Vec::with_capacity(32),
            expr_pool: Arena::with_capacity(2048),
            pattern_pool: Arena::with_capacity(512),
            node_pool: Arena::with_capacity(512),
            interner: Interner::empty(),
        };
        for n in non_comment_children(&n) {
            match n.kind() {
                "definition" => {
                    for n in non_comment_children(&n) {
                        match n.kind() {
                            "top_value_signature" => {
                                m.process_top_value_signature(n)
                            }
                            "top_value_definition" => {}
                            "top_type_signature" => {}
                            "top_type_definition" => {}
                            _ => {
                                m.errs.push(BadCstError::is_unexpected_child(n))
                            }
                        }
                    }
                }
                _ => {
                    let ret = Err(vec![BadCstError::is_unexpected_child(n)]);
                    println!("{:?}", ret);
                    return ret;
                }
            }
        }
        return Err(vec![]);
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
