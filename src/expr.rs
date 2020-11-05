use super::*;
use crate::punctuated::Punctuated;
#[cfg(feature = "full")]
use crate::reserved::Reserved;
use proc_macro2::{Span, TokenStream};
#[cfg(feature = "printing")]
use quote::IdentFragment;
#[cfg(feature = "printing")]
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
#[cfg(feature = "parsing")]
use std::mem;

ast_enum_of_structs! {
    /// A Rust expression.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or `"full"`
    /// feature, but most of the variants are not available unless "full" is enabled.*
    ///
    /// # Syntax tree enums
    ///
    /// This type is a syntax tree enum. In Syn this and other syntax tree enums
    /// are designed to be traversed using the following rebinding idiom.
    ///
    /// ```
    /// # use syn::Expr;
    /// #
    /// # fn example(expr: Expr) {
    /// # const IGNORE: &str = stringify! {
    /// let expr: Expr = /* ... */;
    /// # };
    /// match expr {
    ///     Expr::MethodCall(expr) => {
    ///         /* ... */
    ///     }
    ///     Expr::Cast(expr) => {
    ///         /* ... */
    ///     }
    ///     Expr::If(expr) => {
    ///         /* ... */
    ///     }
    ///
    ///     /* ... */
    ///     # _ => {}
    /// # }
    /// # }
    /// ```
    ///
    /// We begin with a variable `expr` of type `Expr` that has no fields
    /// (because it is an enum), and by matching on it and rebinding a variable
    /// with the same name `expr` we effectively imbue our variable with all of
    /// the data fields provided by the variant that it turned out to be. So for
    /// example above if we ended up in the `MethodCall` case then we get to use
    /// `expr.receiver`, `expr.args` etc; if we ended up in the `If` case we get
    /// to use `expr.cond`, `expr.then_branch`, `expr.else_branch`.
    ///
    /// This approach avoids repeating the variant names twice on every line.
    ///
    /// ```
    /// # use syn::{Expr, ExprMethodCall};
    /// #
    /// # fn example(expr: Expr) {
    /// // Repetitive; recommend not doing this.
    /// match expr {
    ///     Expr::MethodCall(ExprMethodCall { method, args, .. }) => {
    /// # }
    /// # _ => {}
    /// # }
    /// # }
    /// ```
    ///
    /// In general, the name to which a syntax tree enum variant is bound should
    /// be a suitable name for the complete syntax tree enum type.
    ///
    /// ```
    /// # use syn::{Expr, ExprField};
    /// #
    /// # fn example(discriminant: ExprField) {
    /// // Binding is called `base` which is the name I would use if I were
    /// // assigning `*discriminant.base` without an `if let`.
    /// if let Expr::Tuple(base) = *discriminant.base {
    /// # }
    /// # }
    /// ```
    ///
    /// A sign that you may not be choosing the right variable names is if you
    /// see names getting repeated in your code, like accessing
    /// `receiver.receiver` or `pat.pat` or `cond.cond`.
    pub enum Expr {
        /// A slice literal expression: `[a, b, c, d]`.
        Array(ExprArray),

        /// A binary operation: `a + b`, `a * b`.
        Binary(ExprBinary),

        /// A blocked scope: `{ ... }`.
        Block(ExprBlock),

        /// A function call expression: `invoke(a, b)`.
        Call(ExprCall),

        /// A cast expression: `foo as f64`.
        Cast(ExprCast),

        /// Access of a named struct field (`obj.k`) or unnamed tuple struct
        /// field (`obj.0`).
        Field(ExprField),

        /// An expression contained within invisible delimiters.
        ///
        /// This variant is important for faithfully representing the precedence
        /// of expressions and is related to `None`-delimited spans in a
        /// `TokenStream`.
        Group(ExprGroup),

        /// An `if` expression with an optional `else` block: `if expr { ... }
        /// else { ... }`.
        ///
        /// The `else` branch expression may only be an `If` or `Block`
        /// expression, not any of the other types of expression.
        If(ExprIf),

        /// A square bracketed indexing expression: `vector[2]`.
        Index(ExprIndex),

        /// A `let` guard: `let Some(x) = opt`.
        Let(ExprLet),

        /// A literal in place of an expression: `1`, `"foo"`.
        Lit(ExprLit),

        /// A `match` expression: `match n { Some(n) => {}, None => {} }`.
        Match(ExprMatch),

        /// A method call expression: `x.foo::<T>(a, b)`.
        MethodCall(ExprMethodCall),

        /// A parenthesized expression: `(a + b)`.
        Paren(ExprParen),

        /// A path like `std::mem::replace` possibly containing generic
        /// parameters and a qualified self-type.
        ///
        /// A plain identifier like `x` is a path of length 1.
        Path(ExprPath),

        /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
        Range(ExprRange),

        /// A referencing operation: `&a` or `&mut a`.
        Reference(ExprReference),

        /// An array literal constructed from one repeated element: `[0u8; N]`.
        Repeat(ExprRepeat),

        /// A `return`, with an optional value to be returned.
        Return(ExprReturn),

        /// A struct literal expression: `Point { x: 1, y: 1 }`.
        ///
        /// The `rest` provides the value of the remaining fields as in `S { a:
        /// 1, b: 1, ..rest }`.
        Struct(ExprStruct),

        /// A tuple expression: `(a, b, c, d)`.
        Tuple(ExprTuple),

        /// A type ascription expression: `foo: f64`.
        Type(ExprType),

        /// A unary operation: `!x`, `*x`.
        Unary(ExprUnary),

        /// Tokens in expression position not interpreted by Syn.
        Verbatim(TokenStream),

        #[doc(hidden)]
        __Nonexhaustive,
    }
}

ast_struct! {
    /// A slice literal expression: `[a, b, c, d]`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprArray #full {
        pub bracket_token: token::Bracket,
        pub elems: Punctuated<Expr, Token![,]>,
    }
}

ast_struct! {
    /// A binary operation: `a + b`, `a * b`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprBinary {
        pub left: Box<Expr>,
        pub op: BinOp,
        pub right: Box<Expr>,
    }
}

ast_struct! {
    /// A blocked scope: `{ ... }`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprBlock #full {
        pub label: Option<Label>,
        pub block: Block,
    }
}

ast_struct! {
    /// A function call expression: `invoke(a, b)`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprCall {
        pub func: Box<Expr>,
        pub paren_token: token::Paren,
        pub args: Punctuated<Expr, Token![,]>,
    }
}

ast_struct! {
    /// A cast expression: `foo as f64`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprCast {
        pub expr: Box<Expr>,
        pub as_token: Token![as],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    /// Access of a named struct field (`obj.k`) or unnamed tuple struct
    /// field (`obj.0`).
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprField {
        pub base: Box<Expr>,
        pub dot_token: Token![.],
        pub member: Member,
    }
}

ast_struct! {
    /// An expression contained within invisible delimiters.
    ///
    /// This variant is important for faithfully representing the precedence
    /// of expressions and is related to `None`-delimited spans in a
    /// `TokenStream`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprGroup #full {
        pub group_token: token::Group,
        pub expr: Box<Expr>,
    }
}

ast_struct! {
    /// An `if` expression with an optional `else` block: `if expr { ... }
    /// else { ... }`.
    ///
    /// The `else` branch expression may only be an `If` or `Block`
    /// expression, not any of the other types of expression.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprIf #full {
        pub if_token: Token![if],
        pub cond: Box<Expr>,
        pub then_branch: Block,
        pub else_branch: Option<(Token![else], Box<Expr>)>,
    }
}

ast_struct! {
    /// A square bracketed indexing expression: `vector[2]`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprIndex {
        pub expr: Box<Expr>,
        pub bracket_token: token::Bracket,
        pub index: Box<Expr>,
    }
}

ast_struct! {
    /// A `let` guard: `let Some(x) = opt`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprLet #full {
        pub let_token: Token![let],
        pub pat: Pat,
        pub eq_token: Token![=],
        pub expr: Box<Expr>,
    }
}

ast_struct! {
    /// A literal in place of an expression: `1`, `"foo"`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprLit {
        pub lit: Lit,
    }
}

ast_struct! {
    /// A `match` expression: `match n { Some(n) => {}, None => {} }`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprMatch #full {
        pub match_token: Token![match],
        pub expr: Box<Expr>,
        pub brace_token: token::Brace,
        pub arms: Vec<Arm>,
    }
}

ast_struct! {
    /// A method call expression: `x.foo::<T>(a, b)`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprMethodCall #full {
        pub receiver: Box<Expr>,
        pub dot_token: Token![.],
        pub method: Ident,
        pub turbofish: Option<MethodTurbofish>,
        pub paren_token: token::Paren,
        pub args: Punctuated<Expr, Token![,]>,
    }
}

ast_struct! {
    /// A parenthesized expression: `(a + b)`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprParen {
        pub paren_token: token::Paren,
        pub expr: Box<Expr>,
    }
}

ast_struct! {
    /// A path like `std::mem::replace` possibly containing generic
    /// parameters and a qualified self-type.
    ///
    /// A plain identifier like `x` is a path of length 1.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprPath {
        pub qself: Option<QSelf>,
        pub path: Path,
    }
}

ast_struct! {
    /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprRange #full {
        pub from: Option<Box<Expr>>,
        pub limits: RangeLimits,
        pub to: Option<Box<Expr>>,
    }
}

ast_struct! {
    /// A referencing operation: `&a` or `&mut a`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprReference #full {
        pub and_token: Token![&],
        pub raw: Reserved,
        pub mutability: Option<Token![mut]>,
        pub expr: Box<Expr>,
    }
}

ast_struct! {
    /// An array literal constructed from one repeated element: `[0u8; N]`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprRepeat #full {
        pub bracket_token: token::Bracket,
        pub expr: Box<Expr>,
        pub semi_token: Token![;],
        pub len: Box<Expr>,
    }
}

ast_struct! {
    /// A `return`, with an optional value to be returned.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprReturn #full {
        pub return_token: Token![return],
        pub expr: Option<Box<Expr>>,
    }
}

ast_struct! {
    /// A struct literal expression: `Point { x: 1, y: 1 }`.
    ///
    /// The `rest` provides the value of the remaining fields as in `S { a:
    /// 1, b: 1, ..rest }`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprStruct #full {
        pub path: Path,
        pub brace_token: token::Brace,
        pub fields: Punctuated<FieldValue, Token![,]>,
        pub dot2_token: Option<Token![..]>,
        pub rest: Option<Box<Expr>>,
    }
}

ast_struct! {
    /// A tuple expression: `(a, b, c, d)`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprTuple #full {
        pub paren_token: token::Paren,
        pub elems: Punctuated<Expr, Token![,]>,
    }
}

ast_struct! {
    /// A type ascription expression: `foo: f64`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct ExprType #full {
        pub expr: Box<Expr>,
        pub colon_token: Token![:],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    /// A unary operation: `!x`, `*x`.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct ExprUnary {
        pub op: UnOp,
        pub expr: Box<Expr>,
    }
}

ast_enum! {
    /// A struct or tuple struct field accessed in a struct literal or field
    /// expression.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub enum Member {
        /// A named field like `self.x`.
        Named(Ident),
        /// An unnamed field like `self.0`.
        Unnamed(Index),
    }
}

impl Eq for Member {}

impl PartialEq for Member {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Member::Named(this), Member::Named(other)) => this == other,
            (Member::Unnamed(this), Member::Unnamed(other)) => this == other,
            _ => false,
        }
    }
}

impl Hash for Member {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Member::Named(m) => m.hash(state),
            Member::Unnamed(m) => m.hash(state),
        }
    }
}

#[cfg(feature = "printing")]
impl IdentFragment for Member {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Member::Named(m) => Display::fmt(m, formatter),
            Member::Unnamed(m) => Display::fmt(&m.index, formatter),
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            Member::Named(m) => Some(m.span()),
            Member::Unnamed(m) => Some(m.span),
        }
    }
}

ast_struct! {
    /// The index of an unnamed tuple struct field.
    ///
    /// *This type is available only if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct Index {
        pub index: u32,
        pub span: Span,
    }
}

impl From<usize> for Index {
    fn from(index: usize) -> Index {
        assert!(index < u32::max_value() as usize);
        Index {
            index: index as u32,
            span: Span::call_site(),
        }
    }
}

impl Eq for Index {}

impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Hash for Index {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[cfg(feature = "printing")]
impl IdentFragment for Index {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.index, formatter)
    }

    fn span(&self) -> Option<Span> {
        Some(self.span)
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// The `::<>` explicit type parameters passed to a method call:
    /// `parse::<u64>()`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct MethodTurbofish {
        pub colon2_token: Token![::],
        pub lt_token: Token![<],
        pub args: Punctuated<GenericMethodArgument, Token![,]>,
        pub gt_token: Token![>],
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// An individual generic argument to a method, like `T`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub enum GenericMethodArgument {
        /// A type argument.
        Type(Type),
        /// A const expression. Must be inside of a block.
        ///
        /// NOTE: Identity expressions are represented as Type arguments, as
        /// they are indistinguishable syntactically.
        Const(Expr),
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A field-value pair in a struct literal.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct FieldValue {
        /// Attributes tagged on the field.

        /// Name or index of the field.
        pub member: Member,

        /// The colon in `Struct { x: x }`. If written in shorthand like
        /// `Struct { x }`, there is no colon.
        pub colon_token: Option<Token![:]>,

        /// Value of the field.
        pub expr: Expr,
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// A lifetime labeling a `for`, `while`, or `loop`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct Label {
        pub name: Lifetime,
        pub colon_token: Token![:],
    }
}

#[cfg(feature = "full")]
ast_struct! {
    /// One arm of a `match` expression: `0...10 => { return true; }`.
    ///
    /// As in:
    ///
    /// ```
    /// # fn f() -> bool {
    /// #     let n = 0;
    /// match n {
    ///     0...10 => {
    ///         return true;
    ///     }
    ///     // ...
    ///     # _ => {}
    /// }
    /// #   false
    /// # }
    /// ```
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct Arm {
        pub pat: Pat,
        pub guard: Option<(Token![if], Box<Expr>)>,
        pub fat_arrow_token: Token![=>],
        pub body: Box<Expr>,
        pub comma: Option<Token![,]>,
    }
}

#[cfg(feature = "full")]
ast_enum! {
    /// Limit types of a range, inclusive or exclusive.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub enum RangeLimits {
        /// Inclusive at the beginning, exclusive at the end.
        HalfOpen(Token![..]),
        /// Inclusive at the beginning and end.
        Closed(Token![..=]),
    }
}

#[cfg(any(feature = "parsing", feature = "printing"))]
#[cfg(feature = "full")]
pub(crate) fn requires_terminator(expr: &Expr) -> bool {
    // see https://github.com/rust-lang/rust/blob/2679c38fc/src/librustc_ast/util/classify.rs#L7-L25
    match *expr {
        Expr::Block(..)
        | Expr::If(..)
        | Expr::Match(..) => false,
        _ => true,
    }
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use super::*;
    use crate::parse::{Parse, ParseStream, Result};
    use crate::path;
    use std::cmp::Ordering;

    crate::custom_keyword!(raw);

    // When we're parsing expressions which occur before blocks, like in an if
    // statement's condition, we cannot parse a struct literal.
    //
    // Struct literals are ambiguous in certain positions
    // https://github.com/rust-lang/rfcs/pull/92
    pub struct AllowStruct(bool);

    enum Precedence {
        Any,
        Assign,
        Range,
        Or,
        And,
        Compare,
        BitOr,
        BitXor,
        BitAnd,
        Shift,
        Arithmetic,
        Term,
        Cast,
    }

    impl Precedence {
        fn of(op: &BinOp) -> Self {
            match *op {
                BinOp::Add(_) | BinOp::Sub(_) => Precedence::Arithmetic,
                BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => Precedence::Term,
                BinOp::And(_) => Precedence::And,
                BinOp::Or(_) => Precedence::Or,
                BinOp::BitXor(_) => Precedence::BitXor,
                BinOp::BitAnd(_) => Precedence::BitAnd,
                BinOp::BitOr(_) => Precedence::BitOr,
                BinOp::Shl(_) | BinOp::Shr(_) => Precedence::Shift,
                BinOp::Eq(_)
                | BinOp::Lt(_)
                | BinOp::Le(_)
                | BinOp::Ne(_)
                | BinOp::Ge(_)
                | BinOp::Gt(_) => Precedence::Compare,
                BinOp::AddEq(_)
                | BinOp::SubEq(_)
                | BinOp::MulEq(_)
                | BinOp::DivEq(_)
                | BinOp::RemEq(_)
                | BinOp::BitXorEq(_)
                | BinOp::BitAndEq(_)
                | BinOp::BitOrEq(_)
                | BinOp::ShlEq(_)
                | BinOp::ShrEq(_) => Precedence::Assign,
            }
        }
    }

    impl Parse for Expr {
        fn parse(input: ParseStream) -> Result<Self> {
            ambiguous_expr(input, AllowStruct(true))
        }
    }

    impl Expr {
        /// An alternative to the primary `Expr::parse` parser (from the
        /// [`Parse`] trait) for ambiguous syntactic positions in which a
        /// trailing brace should not be taken as part of the expression.
        ///
        /// Rust grammar has an ambiguity where braces sometimes turn a path
        /// expression into a struct initialization and sometimes do not. In the
        /// following code, the expression `S {}` is one expression. Presumably
        /// there is an empty struct `struct S {}` defined somewhere which it is
        /// instantiating.
        ///
        /// ```
        /// # struct S;
        /// # impl std::ops::Deref for S {
        /// #     type Target = bool;
        /// #     fn deref(&self) -> &Self::Target {
        /// #         &true
        /// #     }
        /// # }
        /// let _ = *S {};
        ///
        /// // parsed by rustc as: `*(S {})`
        /// ```
        ///
        /// We would want to parse the above using `Expr::parse` after the `=`
        /// token.
        ///
        /// But in the following, `S {}` is *not* a struct init expression.
        ///
        /// ```
        /// # const S: &bool = &true;
        /// if *S {} {}
        ///
        /// // parsed by rustc as:
        /// //
        /// //    if (*S) {
        /// //        /* empty block */
        /// //    }
        /// //    {
        /// //        /* another empty block */
        /// //    }
        /// ```
        ///
        /// For that reason we would want to parse if-conditions using
        /// `Expr::parse_without_eager_brace` after the `if` token. Same for
        /// similar syntactic positions such as the condition expr after a
        /// `while` token or the expr at the top of a `match`.
        ///
        /// The Rust grammar's choices around which way this ambiguity is
        /// resolved at various syntactic positions is fairly arbitrary. Really
        /// either parse behavior could work in most positions, and language
        /// designers just decide each case based on which is more likely to be
        /// what the programmer had in mind most of the time.
        ///
        /// ```
        /// # struct S;
        /// # fn doc() -> S {
        /// if return S {} {}
        /// # unreachable!()
        /// # }
        ///
        /// // parsed by rustc as:
        /// //
        /// //    if (return (S {})) {
        /// //    }
        /// //
        /// // but could equally well have been this other arbitrary choice:
        /// //
        /// //    if (return S) {
        /// //    }
        /// //    {}
        /// ```
        ///
        /// Note the grammar ambiguity on trailing braces is distinct from
        /// precedence and is not captured by assigning a precedence level to
        /// the braced struct init expr in relation to other operators. This can
        /// be illustrated by `return 0..S {}` vs `match 0..S {}`. The former
        /// parses as `return (0..(S {}))` implying tighter precedence for
        /// struct init than `..`, while the latter parses as `match (0..S) {}`
        /// implying tighter precedence for `..` than struct init, a
        /// contradiction.
        #[cfg(feature = "full")]
        pub fn parse_without_eager_brace(input: ParseStream) -> Result<Expr> {
            ambiguous_expr(input, AllowStruct(false))
        }
    }

    impl Copy for AllowStruct {}

    impl Clone for AllowStruct {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Copy for Precedence {}

    impl Clone for Precedence {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl PartialEq for Precedence {
        fn eq(&self, other: &Self) -> bool {
            *self as u8 == *other as u8
        }
    }

    impl PartialOrd for Precedence {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            let this = *self as u8;
            let other = *other as u8;
            Some(this.cmp(&other))
        }
    }

    #[cfg(feature = "full")]
    fn parse_expr(
        input: ParseStream,
        mut lhs: Expr,
        allow_struct: AllowStruct,
        base: Precedence,
    ) -> Result<Expr> {
        loop {
            if input
                .fork()
                .parse::<BinOp>()
                .ok()
                .map_or(false, |op| Precedence::of(&op) >= base)
            {
                let op: BinOp = input.parse()?;
                let precedence = Precedence::of(&op);
                let mut rhs = unary_expr(input, allow_struct)?;
                loop {
                    let next = peek_precedence(input);
                    if next > precedence || next == precedence && precedence == Precedence::Assign {
                        rhs = parse_expr(input, rhs, allow_struct, next)?;
                    } else {
                        break;
                    }
                }
                lhs = {
                    Expr::Binary(ExprBinary {
                        left: Box::new(lhs),
                        op,
                        right: Box::new(rhs),
                    })
                };
            } else if Precedence::Range >= base && input.peek(Token![..]) {
                let limits: RangeLimits = input.parse()?;
                let rhs = if input.is_empty()
                    || input.peek(Token![,])
                    || input.peek(Token![;])
                    || !allow_struct.0 && input.peek(token::Brace)
                {
                    None
                } else {
                    let mut rhs = unary_expr(input, allow_struct)?;
                    loop {
                        let next = peek_precedence(input);
                        if next > Precedence::Range {
                            rhs = parse_expr(input, rhs, allow_struct, next)?;
                        } else {
                            break;
                        }
                    }
                    Some(rhs)
                };
                lhs = Expr::Range(ExprRange {
                    from: Some(Box::new(lhs)),
                    limits,
                    to: rhs.map(Box::new),
                });
            } else if Precedence::Cast >= base && input.peek(Token![as]) {
                let as_token: Token![as] = input.parse()?;
                let ty = input.call(Type::without_plus)?;
                lhs = Expr::Cast(ExprCast {
                    expr: Box::new(lhs),
                    as_token,
                    ty: Box::new(ty),
                });
            } else if Precedence::Cast >= base && input.peek(Token![:]) && !input.peek(Token![::]) {
                let colon_token: Token![:] = input.parse()?;
                let ty = input.call(Type::without_plus)?;
                lhs = Expr::Type(ExprType {
                    expr: Box::new(lhs),
                    colon_token,
                    ty: Box::new(ty),
                });
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    #[cfg(not(feature = "full"))]
    fn parse_expr(
        input: ParseStream,
        mut lhs: Expr,
        allow_struct: AllowStruct,
        base: Precedence,
    ) -> Result<Expr> {
        loop {
            if input
                .fork()
                .parse::<BinOp>()
                .ok()
                .map_or(false, |op| Precedence::of(&op) >= base)
            {
                let op: BinOp = input.parse()?;
                let precedence = Precedence::of(&op);
                let mut rhs = unary_expr(input, allow_struct)?;
                loop {
                    let next = peek_precedence(input);
                    if next > precedence || next == precedence && precedence == Precedence::Assign {
                        rhs = parse_expr(input, rhs, allow_struct, next)?;
                    } else {
                        break;
                    }
                }
                lhs = Expr::Binary(ExprBinary {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                });
            } else if Precedence::Cast >= base && input.peek(Token![as]) {
                let as_token: Token![as] = input.parse()?;
                let ty = input.call(Type::without_plus)?;
                lhs = Expr::Cast(ExprCast {
                    expr: Box::new(lhs),
                    as_token,
                    ty: Box::new(ty),
                });
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn peek_precedence(input: ParseStream) -> Precedence {
        if let Ok(op) = input.fork().parse() {
            Precedence::of(&op)
        } else if input.peek(Token![=]) && !input.peek(Token![=>]) {
            Precedence::Assign
        } else if input.peek(Token![..]) {
            Precedence::Range
        } else if input.peek(Token![as]) || input.peek(Token![:]) && !input.peek(Token![::]) {
            Precedence::Cast
        } else {
            Precedence::Any
        }
    }

    // Parse an arbitrary expression.
    fn ambiguous_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        let lhs = unary_expr(input, allow_struct)?;
        parse_expr(input, lhs, allow_struct, Precedence::Any)
    }

    // <UnOp> <trailer>
    // & <trailer>
    // &mut <trailer>
    // box <trailer>
    #[cfg(feature = "full")]
    fn unary_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        let begin = input.fork();
        if input.peek(Token![&]) {
            let and_token: Token![&] = input.parse()?;
            let raw: Option<raw> =
                if input.peek(raw) && (input.peek2(Token![mut]) || input.peek2(Token![const])) {
                    Some(input.parse()?)
                } else {
                    None
                };
            let mutability: Option<Token![mut]> = input.parse()?;
            if raw.is_some() && mutability.is_none() {
                input.parse::<Token![const]>()?;
            }
            let expr = Box::new(unary_expr(input, allow_struct)?);
            if raw.is_some() {
                Ok(Expr::Verbatim(verbatim::between(begin, input)))
            } else {
                Ok(Expr::Reference(ExprReference {
                    and_token,
                    raw: Reserved::default(),
                    mutability,
                    expr,
                }))
            }
        } else if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
            Ok(Expr::Unary(ExprUnary {
                op: input.parse()?,
                expr: Box::new(unary_expr(input, allow_struct)?),
            }))
        } else {
            trailer_expr(input, allow_struct)
        }
    }

    #[cfg(not(feature = "full"))]
    fn unary_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
            Ok(Expr::Unary(ExprUnary {
                op: input.parse()?,
                expr: Box::new(unary_expr(input, allow_struct)?),
            }))
        } else {
            trailer_expr(input, allow_struct)
        }
    }

    // <atom> (..<args>) ...
    // <atom> . <ident> (..<args>) ...
    // <atom> . <ident> ...
    // <atom> . <lit> ...
    // <atom> [ <expr> ] ...
    // <atom> ? ...
    #[cfg(feature = "full")]
    fn trailer_expr(
        input: ParseStream,
        allow_struct: AllowStruct,
    ) -> Result<Expr> {
        let atom = atom_expr(input, allow_struct)?;
        let e = trailer_helper(input, atom)?;
        Ok(e)
    }

    #[cfg(feature = "full")]
    fn trailer_helper(input: ParseStream, mut e: Expr) -> Result<Expr> {
        loop {
            if input.peek(token::Paren) {
                let content;
                e = Expr::Call(ExprCall {
                    func: Box::new(e),
                    paren_token: parenthesized!(content in input),
                    args: content.parse_terminated(Expr::parse)?,
                });
            } else if input.peek(Token![.]) && !input.peek(Token![..]) {
                let mut dot_token: Token![.] = input.parse()?;

                let float_token: Option<LitFloat> = input.parse()?;
                if let Some(float_token) = float_token {
                    if multi_index(&mut e, &mut dot_token, float_token)? {
                        continue;
                    }
                }

                let member: Member = input.parse()?;
                let turbofish = if member.is_named() && input.peek(Token![::]) {
                    Some(MethodTurbofish {
                        colon2_token: input.parse()?,
                        lt_token: input.parse()?,
                        args: {
                            let mut args = Punctuated::new();
                            loop {
                                if input.peek(Token![>]) {
                                    break;
                                }
                                let value = input.call(generic_method_argument)?;
                                args.push_value(value);
                                if input.peek(Token![>]) {
                                    break;
                                }
                                let punct = input.parse()?;
                                args.push_punct(punct);
                            }
                            args
                        },
                        gt_token: input.parse()?,
                    })
                } else {
                    None
                };

                if turbofish.is_some() || input.peek(token::Paren) {
                    if let Member::Named(method) = member {
                        let content;
                        e = Expr::MethodCall(ExprMethodCall {
                            receiver: Box::new(e),
                            dot_token,
                            method,
                            turbofish,
                            paren_token: parenthesized!(content in input),
                            args: content.parse_terminated(Expr::parse)?,
                        });
                        continue;
                    }
                }

                e = Expr::Field(ExprField {
                    base: Box::new(e),
                    dot_token,
                    member,
                });
            } else if input.peek(token::Bracket) {
                let content;
                e = Expr::Index(ExprIndex {
                    expr: Box::new(e),
                    bracket_token: bracketed!(content in input),
                    index: content.parse()?,
                });
            } else {
                break;
            }
        }
        Ok(e)
    }

    #[cfg(not(feature = "full"))]
    fn trailer_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        let mut e = atom_expr(input, allow_struct)?;

        loop {
            if input.peek(token::Paren) {
                let content;
                e = Expr::Call(ExprCall {
                    func: Box::new(e),
                    paren_token: parenthesized!(content in input),
                    args: content.parse_terminated(Expr::parse)?,
                });
            } else if input.peek(Token![.]) && !input.peek(Token![..]) && !input.peek2(token::Await)
            {
                let mut dot_token: Token![.] = input.parse()?;
                let float_token: Option<LitFloat> = input.parse()?;
                if let Some(float_token) = float_token {
                    if multi_index(&mut e, &mut dot_token, float_token)? {
                        continue;
                    }
                }
                e = Expr::Field(ExprField {
                    base: Box::new(e),
                    dot_token,
                    member: input.parse()?,
                });
            } else if input.peek(token::Bracket) {
                let content;
                e = Expr::Index(ExprIndex {
                    expr: Box::new(e),
                    bracket_token: bracketed!(content in input),
                    index: content.parse()?,
                });
            } else {
                break;
            }
        }

        Ok(e)
    }

    // Parse all atomic expressions which don't have to worry about precedence
    // interactions, as they are fully contained.
    #[cfg(feature = "full")]
    fn atom_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        if input.peek(token::Group)
            && !input.peek2(Token![::])
            && !input.peek2(Token![!])
            && !input.peek2(token::Brace)
        {
            input.call(expr_group).map(Expr::Group)
        } else if input.peek(Lit) {
            input.parse().map(Expr::Lit)
        } else if input.peek(Ident)
            || input.peek(Token![::])
            || input.peek(Token![<])
            || input.peek(Token![self])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            path_or_struct(input, allow_struct)
        } else if input.peek(token::Paren) {
            paren_or_tuple(input)
        } else if input.peek(Token![return]) {
            expr_ret(input, allow_struct).map(Expr::Return)
        } else if input.peek(token::Bracket) {
            array_or_repeat(input)
        } else if input.peek(Token![let]) {
            input.call(expr_let).map(Expr::Let)
        } else if input.peek(Token![if]) {
            input.parse().map(Expr::If)
        } else if input.peek(Token![match]) {
            input.parse().map(Expr::Match)
        } else if input.peek(token::Brace) {
            input.call(expr_block).map(Expr::Block)
        } else if input.peek(Token![..]) {
            expr_range(input, allow_struct).map(Expr::Range)
        } else if input.peek(Lifetime) {
            let the_label: Label = input.parse()?;
            let mut expr = if input.peek(token::Brace) {
                Expr::Block(input.call(expr_block)?)
            } else {
                return Err(input.error("expected loop or block expression"));
            };
            match &mut expr {
                Expr::Block(ExprBlock { label, .. }) => *label = Some(the_label),
                _ => unreachable!(),
            }
            Ok(expr)
        } else {
            Err(input.error("expected expression"))
        }
    }

    #[cfg(not(feature = "full"))]
    fn atom_expr(input: ParseStream, _allow_struct: AllowStruct) -> Result<Expr> {
        if input.peek(Lit) {
            input.parse().map(Expr::Lit)
        } else if input.peek(token::Paren) {
            input.call(expr_paren).map(Expr::Paren)
        } else if input.peek(Ident)
            || input.peek(Token![::])
            || input.peek(Token![<])
            || input.peek(Token![self])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            input.parse().map(Expr::Path)
        } else {
            Err(input.error("unsupported expression; enable syn's features=[\"full\"]"))
        }
    }

    #[cfg(feature = "full")]
    fn path_or_struct(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        let expr: ExprPath = input.parse()?;
        if expr.qself.is_some() {
            return Ok(Expr::Path(expr));
        }

        if allow_struct.0 && input.peek(token::Brace) {
            expr_struct_helper(input, expr.path).map(Expr::Struct)
        } else {
            Ok(Expr::Path(expr))
        }
    }

    #[cfg(feature = "full")]
    fn paren_or_tuple(input: ParseStream) -> Result<Expr> {
        let content;
        let paren_token = parenthesized!(content in input);
        if content.is_empty() {
            return Ok(Expr::Tuple(ExprTuple {
                paren_token,
                elems: Punctuated::new(),
            }));
        }

        let first: Expr = content.parse()?;
        if content.is_empty() {
            return Ok(Expr::Paren(ExprParen {
                paren_token,
                expr: Box::new(first),
            }));
        }

        let mut elems = Punctuated::new();
        elems.push_value(first);
        while !content.is_empty() {
            let punct = content.parse()?;
            elems.push_punct(punct);
            if content.is_empty() {
                break;
            }
            let value = content.parse()?;
            elems.push_value(value);
        }
        Ok(Expr::Tuple(ExprTuple {
            paren_token,
            elems,
        }))
    }

    #[cfg(feature = "full")]
    fn array_or_repeat(input: ParseStream) -> Result<Expr> {
        let content;
        let bracket_token = bracketed!(content in input);
        if content.is_empty() {
            return Ok(Expr::Array(ExprArray {
                bracket_token,
                elems: Punctuated::new(),
            }));
        }

        let first: Expr = content.parse()?;
        if content.is_empty() || content.peek(Token![,]) {
            let mut elems = Punctuated::new();
            elems.push_value(first);
            while !content.is_empty() {
                let punct = content.parse()?;
                elems.push_punct(punct);
                if content.is_empty() {
                    break;
                }
                let value = content.parse()?;
                elems.push_value(value);
            }
            Ok(Expr::Array(ExprArray {
                bracket_token,
                elems,
            }))
        } else if content.peek(Token![;]) {
            let semi_token: Token![;] = content.parse()?;
            let len: Expr = content.parse()?;
            Ok(Expr::Repeat(ExprRepeat {
                bracket_token,
                expr: Box::new(first),
                semi_token,
                len: Box::new(len),
            }))
        } else {
            Err(content.error("expected `,` or `;`"))
        }
    }

    #[cfg(feature = "full")]
    pub(crate) fn expr_early(input: ParseStream) -> Result<Expr> {
        let mut expr = if input.peek(Token![if]) {
            Expr::If(input.parse()?)
        } else if input.peek(Token![match]) {
            Expr::Match(input.parse()?)
        } else if input.peek(token::Brace) {
            Expr::Block(input.call(expr_block)?)
        } else {
            let allow_struct = AllowStruct(true);
            let expr = unary_expr(input, allow_struct)?;

            return parse_expr(input, expr, allow_struct, Precedence::Any);
        };

        if input.peek(Token![.]) && !input.peek(Token![..]) || input.peek(Token![?]) {
            expr = trailer_helper(input, expr)?;

            let allow_struct = AllowStruct(true);
            return parse_expr(input, expr, allow_struct, Precedence::Any);
        }

        Ok(expr)
    }

    impl Parse for ExprLit {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprLit {
                lit: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_group(input: ParseStream) -> Result<ExprGroup> {
        let group = crate::group::parse_group(input)?;
        Ok(ExprGroup {
            group_token: group.token,
            expr: group.content.parse()?,
        })
    }

    #[cfg(not(feature = "full"))]
    fn expr_paren(input: ParseStream) -> Result<ExprParen> {
        let content;
        Ok(ExprParen {
            paren_token: parenthesized!(content in input),
            expr: content.parse()?,
        })
    }

    #[cfg(feature = "full")]
    fn generic_method_argument(input: ParseStream) -> Result<GenericMethodArgument> {
        if input.peek(Lit) {
            let lit = input.parse()?;
            return Ok(GenericMethodArgument::Const(Expr::Lit(lit)));
        }

        if input.peek(token::Brace) {
            let block = input.call(expr::parsing::expr_block)?;
            return Ok(GenericMethodArgument::Const(Expr::Block(block)));
        }

        input.parse().map(GenericMethodArgument::Type)
    }

    #[cfg(feature = "full")]
    fn expr_let(input: ParseStream) -> Result<ExprLet> {
        Ok(ExprLet {
            let_token: input.parse()?,
            pat: pat::parsing::multi_pat_with_leading_vert(input)?,
            eq_token: input.parse()?,
            expr: Box::new(input.call(Expr::parse_without_eager_brace)?),
        })
    }

    #[cfg(feature = "full")]
    impl Parse for ExprIf {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprIf {
                if_token: input.parse()?,
                cond: Box::new(input.call(Expr::parse_without_eager_brace)?),
                then_branch: input.parse()?,
                else_branch: {
                    if input.peek(Token![else]) {
                        Some(input.call(else_block)?)
                    } else {
                        None
                    }
                },
            })
        }
    }

    #[cfg(feature = "full")]
    fn else_block(input: ParseStream) -> Result<(Token![else], Box<Expr>)> {
        let else_token: Token![else] = input.parse()?;

        let lookahead = input.lookahead1();
        let else_branch = if input.peek(Token![if]) {
            input.parse().map(Expr::If)?
        } else if input.peek(token::Brace) {
            Expr::Block(ExprBlock {
                label: None,
                block: input.parse()?,
            })
        } else {
            return Err(lookahead.error());
        };

        Ok((else_token, Box::new(else_branch)))
    }

    #[cfg(feature = "full")]
    impl Parse for ExprMatch {
        fn parse(input: ParseStream) -> Result<Self> {
            let match_token: Token![match] = input.parse()?;
            let expr = Expr::parse_without_eager_brace(input)?;

            let content;
            let brace_token = braced!(content in input);

            let mut arms = Vec::new();
            while !content.is_empty() {
                arms.push(content.call(Arm::parse)?);
            }

            Ok(ExprMatch {
                match_token,
                expr: Box::new(expr),
                brace_token,
                arms,
            })
        }
    }

    macro_rules! impl_by_parsing_expr {
        (
            $(
                $expr_type:ty, $variant:ident, $msg:expr,
            )*
        ) => {
            $(
                #[cfg(all(feature = "full", feature = "printing"))]
                impl Parse for $expr_type {
                    fn parse(input: ParseStream) -> Result<Self> {
                        let mut expr: Expr = input.parse()?;
                        loop {
                            match expr {
                                Expr::$variant(inner) => return Ok(inner),
                                Expr::Group(next) => expr = *next.expr,
                                _ => return Err(Error::new_spanned(expr, $msg)),
                            }
                        }
                    }
                }
            )*
        };
    }

    impl_by_parsing_expr! {
        ExprArray, Array, "expected slice literal expression",
        ExprCall, Call, "expected function call expression",
        ExprMethodCall, MethodCall, "expected method call expression",
        ExprTuple, Tuple, "expected tuple expression",
        ExprBinary, Binary, "expected binary operation",
        ExprUnary, Unary, "expected unary operation",
        ExprCast, Cast, "expected cast expression",
        ExprType, Type, "expected type ascription expression",
        ExprLet, Let, "expected let guard",
        ExprField, Field, "expected struct field access",
        ExprIndex, Index, "expected indexing expression",
        ExprRange, Range, "expected range expression",
        ExprReference, Reference, "expected referencing operation",
        ExprReturn, Return, "expected return expression",
        ExprStruct, Struct, "expected struct literal expression",
        ExprRepeat, Repeat, "expected array literal constructed from one repeated element",
        ExprParen, Paren, "expected parenthesized expression",
    }

    #[cfg(feature = "full")]
    impl Parse for Label {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Label {
                name: input.parse()?,
                colon_token: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    impl Parse for Option<Label> {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Lifetime) {
                input.parse().map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[cfg(feature = "full")]
    fn expr_ret(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprReturn> {
        Ok(ExprReturn {
            return_token: input.parse()?,
            expr: {
                if input.is_empty() || input.peek(Token![,]) || input.peek(Token![;]) {
                    None
                } else {
                    // NOTE: return is greedy and eats blocks after it even when in a
                    // position where structs are not allowed, such as in if statement
                    // conditions. For example:
                    //
                    // if return { println!("A") } {} // Prints "A"
                    let expr = ambiguous_expr(input, allow_struct)?;
                    Some(Box::new(expr))
                }
            },
        })
    }

    #[cfg(feature = "full")]
    impl Parse for FieldValue {
        fn parse(input: ParseStream) -> Result<Self> {
            let member: Member = input.parse()?;
            let (colon_token, value) = if input.peek(Token![:]) || !member.is_named() {
                let colon_token: Token![:] = input.parse()?;
                let value: Expr = input.parse()?;
                (Some(colon_token), value)
            } else if let Member::Named(ident) = &member {
                let value = Expr::Path(ExprPath {
                    qself: None,
                    path: Path::from(ident.clone()),
                });
                (None, value)
            } else {
                unreachable!()
            };

            Ok(FieldValue {
                member,
                colon_token,
                expr: value,
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_struct_helper(
        input: ParseStream,
        path: Path,
    ) -> Result<ExprStruct> {
        let content;
        let brace_token = braced!(content in input);

        let mut fields = Punctuated::new();
        while !content.is_empty() {
            if content.peek(Token![..]) {
                return Ok(ExprStruct {
                    brace_token,
                    path,
                    fields,
                    dot2_token: Some(content.parse()?),
                    rest: Some(Box::new(content.parse()?)),
                });
            }

            fields.push(content.parse()?);
            if content.is_empty() {
                break;
            }
            let punct: Token![,] = content.parse()?;
            fields.push_punct(punct);
        }

        Ok(ExprStruct {
            brace_token,
            path,
            fields,
            dot2_token: None,
            rest: None,
        })
    }

    #[cfg(feature = "full")]
    pub fn expr_block(input: ParseStream) -> Result<ExprBlock> {
        let label: Option<Label> = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        let stmts = content.call(Block::parse_within)?;

        Ok(ExprBlock {
            label,
            block: Block { brace_token, stmts },
        })
    }

    #[cfg(feature = "full")]
    fn expr_range(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprRange> {
        Ok(ExprRange {
            from: None,
            limits: input.parse()?,
            to: {
                if input.is_empty()
                    || input.peek(Token![,])
                    || input.peek(Token![;])
                    || !allow_struct.0 && input.peek(token::Brace)
                {
                    None
                } else {
                    let to = ambiguous_expr(input, allow_struct)?;
                    Some(Box::new(to))
                }
            },
        })
    }

    #[cfg(feature = "full")]
    impl Parse for RangeLimits {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![..=]) {
                input.parse().map(RangeLimits::Closed)
            } else if lookahead.peek(Token![...]) {
                let dot3: Token![...] = input.parse()?;
                Ok(RangeLimits::Closed(Token![..=](dot3.spans)))
            } else if lookahead.peek(Token![..]) {
                input.parse().map(RangeLimits::HalfOpen)
            } else {
                Err(lookahead.error())
            }
        }
    }

    impl Parse for ExprPath {
        fn parse(input: ParseStream) -> Result<Self> {
            let (qself, path) = path::parsing::qpath(input, true)?;
            Ok(ExprPath { qself, path })
        }
    }

    impl Parse for Member {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Ident) {
                input.parse().map(Member::Named)
            } else if input.peek(LitInt) {
                input.parse().map(Member::Unnamed)
            } else {
                Err(input.error("expected identifier or integer"))
            }
        }
    }

    #[cfg(feature = "full")]
    impl Parse for Arm {
        fn parse(input: ParseStream) -> Result<Arm> {
            let requires_comma;
            Ok(Arm {
                pat: pat::parsing::multi_pat_with_leading_vert(input)?,
                guard: {
                    if input.peek(Token![if]) {
                        let if_token: Token![if] = input.parse()?;
                        let guard: Expr = input.parse()?;
                        Some((if_token, Box::new(guard)))
                    } else {
                        None
                    }
                },
                fat_arrow_token: input.parse()?,
                body: {
                    let body = input.call(expr_early)?;
                    requires_comma = requires_terminator(&body);
                    Box::new(body)
                },
                comma: {
                    if requires_comma && !input.is_empty() {
                        Some(input.parse()?)
                    } else {
                        input.parse()?
                    }
                },
            })
        }
    }

    impl Parse for Index {
        fn parse(input: ParseStream) -> Result<Self> {
            let lit: LitInt = input.parse()?;
            if lit.suffix().is_empty() {
                Ok(Index {
                    index: lit
                        .base10_digits()
                        .parse()
                        .map_err(|err| Error::new(lit.span(), err))?,
                    span: lit.span(),
                })
            } else {
                Err(Error::new(lit.span(), "expected unsuffixed integer"))
            }
        }
    }

    fn multi_index(e: &mut Expr, dot_token: &mut Token![.], float: LitFloat) -> Result<bool> {
        let mut float_repr = float.to_string();
        let trailing_dot = float_repr.ends_with('.');
        if trailing_dot {
            float_repr.truncate(float_repr.len() - 1);
        }
        for part in float_repr.split('.') {
            let index = crate::parse_str(part).map_err(|err| Error::new(float.span(), err))?;
            let base = mem::replace(e, Expr::__Nonexhaustive);
            *e = Expr::Field(ExprField {
                base: Box::new(base),
                dot_token: Token![.](dot_token.span),
                member: Member::Unnamed(index),
            });
            *dot_token = Token![.](float.span());
        }
        Ok(!trailing_dot)
    }

    #[cfg(feature = "full")]
    impl Member {
        fn is_named(&self) -> bool {
            match *self {
                Member::Named(_) => true,
                Member::Unnamed(_) => false,
            }
        }
    }
}

#[cfg(feature = "printing")]
pub(crate) mod printing {
    use super::*;
    #[cfg(feature = "full")]
    use crate::print::TokensOrDefault;
    use proc_macro2::{Literal, TokenStream};
    use quote::{ToTokens, TokenStreamExt};

    // If the given expression is a bare `ExprStruct`, wraps it in parenthesis
    // before appending it to `TokenStream`.
    #[cfg(feature = "full")]
    fn wrap_bare_struct(tokens: &mut TokenStream, e: &Expr) {
        if let Expr::Struct(_) = *e {
            token::Paren::default().surround(tokens, |tokens| {
                e.to_tokens(tokens);
            });
        } else {
            e.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprArray {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.bracket_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for ExprCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.func.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.receiver.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.method.to_tokens(tokens);
            self.turbofish.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.args.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for MethodTurbofish {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.colon2_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.args.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for GenericMethodArgument {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                GenericMethodArgument::Type(t) => t.to_tokens(tokens),
                GenericMethodArgument::Const(c) => c.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprTuple {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
                // If we only have one argument, we need a trailing comma to
                // distinguish ExprTuple from ExprParen.
                if self.elems.len() == 1 && !self.elems.trailing_punct() {
                    <Token![,]>::default().to_tokens(tokens);
                }
            })
        }
    }

    impl ToTokens for ExprBinary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprUnary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.op.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprLit {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.lit.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprCast {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.expr.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.expr.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    fn maybe_wrap_else(tokens: &mut TokenStream, else_: &Option<(Token![else], Box<Expr>)>) {
        if let Some((else_token, else_)) = else_ {
            else_token.to_tokens(tokens);

            // If we are not one of the valid expressions to exist in an else
            // clause, wrap ourselves in a block.
            match **else_ {
                Expr::If(_) | Expr::Block(_) => {
                    else_.to_tokens(tokens);
                }
                _ => {
                    token::Brace::default().surround(tokens, |tokens| {
                        else_.to_tokens(tokens);
                    });
                }
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprLet {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.if_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.cond);
            self.then_branch.to_tokens(tokens);
            maybe_wrap_else(tokens, &self.else_branch);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprMatch {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.match_token.to_tokens(tokens);
            wrap_bare_struct(tokens, &self.expr);
            self.brace_token.surround(tokens, |tokens| {
                for (i, arm) in self.arms.iter().enumerate() {
                    arm.to_tokens(tokens);
                    // Ensure that we have a comma after a non-block arm, except
                    // for the last one.
                    let is_last = i == self.arms.len() - 1;
                    if !is_last && requires_terminator(&arm.body) && arm.comma.is_none() {
                        <Token![,]>::default().to_tokens(tokens);
                    }
                }
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.label.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.base.to_tokens(tokens);
            self.dot_token.to_tokens(tokens);
            self.member.to_tokens(tokens);
        }
    }

    impl ToTokens for Member {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Member::Named(ident) => ident.to_tokens(tokens),
                Member::Unnamed(index) => index.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for Index {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let mut lit = Literal::i64_unsuffixed(i64::from(self.index));
            lit.set_span(self.span);
            tokens.append(lit);
        }
    }

    impl ToTokens for ExprIndex {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.expr.to_tokens(tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.index.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRange {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.from.to_tokens(tokens);
            match &self.limits {
                RangeLimits::HalfOpen(t) => t.to_tokens(tokens),
                RangeLimits::Closed(t) => t.to_tokens(tokens),
            }
            self.to.to_tokens(tokens);
        }
    }

    impl ToTokens for ExprPath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            private::print_path(tokens, &self.qself, &self.path);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprReference {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.and_token.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprReturn {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.return_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                if self.rest.is_some() {
                    TokensOrDefault(&self.dot2_token).to_tokens(tokens);
                    self.rest.to_tokens(tokens);
                }
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprRepeat {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.bracket_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.len.to_tokens(tokens);
            })
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for ExprGroup {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.group_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Label {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.name.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.member.to_tokens(tokens);
            if let Some(colon_token) = &self.colon_token {
                colon_token.to_tokens(tokens);
                self.expr.to_tokens(tokens);
            }
        }
    }

    #[cfg(feature = "full")]
    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.pat.to_tokens(tokens);
            if let Some((if_token, guard)) = &self.guard {
                if_token.to_tokens(tokens);
                guard.to_tokens(tokens);
            }
            self.fat_arrow_token.to_tokens(tokens);
            self.body.to_tokens(tokens);
            self.comma.to_tokens(tokens);
        }
    }
}
