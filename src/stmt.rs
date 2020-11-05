use super::*;

ast_struct! {
    /// A braced block containing Rust statements.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct Block {
        pub brace_token: token::Brace,
        /// Statements in a block
        pub stmts: Vec<Stmt>,
    }
}

ast_enum! {
    /// A statement, usually ending in a semicolon.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub enum Stmt {
        /// A local (let) binding.
        Local(Local),

        /// Expr without trailing semicolon.
        Expr(Expr),

        /// Expression with trailing semicolon.
        Semi(Expr, Token![;]),
    }
}

ast_struct! {
    /// A local `let` binding: `let x: u64 = s.parse()?`.
    ///
    /// *This type is available only if Syn is built with the `"full"` feature.*
    pub struct Local {
        pub attrs: Vec<Attribute>,
        pub let_token: Token![let],
        pub pat: Pat,
        pub init: Option<(Token![=], Box<Expr>)>,
        pub semi_token: Token![;],
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use crate::parse::{Parse, ParseStream, Result};
    use proc_macro2::TokenStream;

    impl Block {
        /// Parse the body of a block as zero or more statements, possibly
        /// including one trailing expression.
        ///
        /// *This function is available only if Syn is built with the `"parsing"`
        /// feature.*
        ///
        /// # Example
        ///
        /// ```
        /// use syn::{braced, token, Attribute, Block, Ident, Result, Stmt, Token};
        /// use syn::parse::{Parse, ParseStream};
        ///
        /// // Parse a function with no generics or parameter list.
        /// //
        /// //     fn playground {
        /// //         let mut x = 1;
        /// //         x += 1;
        /// //         println!("{}", x);
        /// //     }
        /// struct MiniFunction {
        ///     attrs: Vec<Attribute>,
        ///     fn_token: Token![fn],
        ///     name: Ident,
        ///     brace_token: token::Brace,
        ///     stmts: Vec<Stmt>,
        /// }
        ///
        /// impl Parse for MiniFunction {
        ///     fn parse(input: ParseStream) -> Result<Self> {
        ///         let outer_attrs = input.call(Attribute::parse_outer)?;
        ///         let fn_token: Token![fn] = input.parse()?;
        ///         let name: Ident = input.parse()?;
        ///
        ///         let content;
        ///         let brace_token = braced!(content in input);
        ///         let inner_attrs = content.call(Attribute::parse_inner)?;
        ///         let stmts = content.call(Block::parse_within)?;
        ///
        ///         Ok(MiniFunction {
        ///             attrs: {
        ///                 let mut attrs = outer_attrs;
        ///                 attrs.extend(inner_attrs);
        ///                 attrs
        ///             },
        ///             fn_token,
        ///             name,
        ///             brace_token,
        ///             stmts,
        ///         })
        ///     }
        /// }
        /// ```
        pub fn parse_within(input: ParseStream) -> Result<Vec<Stmt>> {
            let mut stmts = Vec::new();
            loop {
                while let Some(semi) = input.parse::<Option<Token![;]>>()? {
                    stmts.push(Stmt::Semi(Expr::Verbatim(TokenStream::new()), semi));
                }
                if input.is_empty() {
                    break;
                }
                let s = parse_stmt(input, true)?;
                let requires_semicolon = if let Stmt::Expr(s) = &s {
                    expr::requires_terminator(s)
                } else {
                    false
                };
                stmts.push(s);
                if input.is_empty() {
                    break;
                } else if requires_semicolon {
                    return Err(input.error("unexpected token"));
                }
            }
            Ok(stmts)
        }
    }

    impl Parse for Block {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(Block {
                brace_token: braced!(content in input),
                stmts: content.call(Block::parse_within)?,
            })
        }
    }

    impl Parse for Stmt {
        fn parse(input: ParseStream) -> Result<Self> {
            parse_stmt(input, false)
        }
    }

    fn parse_stmt(input: ParseStream, allow_nosemi: bool) -> Result<Stmt> {
        let mut attrs = input.call(Attribute::parse_outer)?;

        if input.peek(Token![let]) {
            stmt_local(input, attrs).map(Stmt::Local)
        } else {
            stmt_expr(input, allow_nosemi, attrs)
        }
    }

    fn stmt_local(input: ParseStream, attrs: Vec<Attribute>) -> Result<Local> {
        Ok(Local {
            attrs,
            let_token: input.parse()?,
            pat: {
                let mut pat: Pat = pat::parsing::multi_pat_with_leading_vert(input)?;
                if input.peek(Token![:]) {
                    let colon_token: Token![:] = input.parse()?;
                    let ty: Type = input.parse()?;
                    pat = Pat::Type(PatType {
                        attrs: Vec::new(),
                        pat: Box::new(pat),
                        colon_token,
                        ty: Box::new(ty),
                    });
                }
                pat
            },
            init: {
                if input.peek(Token![=]) {
                    let eq_token: Token![=] = input.parse()?;
                    let init: Expr = input.parse()?;
                    Some((eq_token, Box::new(init)))
                } else {
                    None
                }
            },
            semi_token: input.parse()?,
        })
    }

    fn stmt_expr(
        input: ParseStream,
        allow_nosemi: bool,
        mut attrs: Vec<Attribute>,
    ) -> Result<Stmt> {
        let mut e = expr::parsing::expr_early(input)?;

        let mut attr_target = &mut e;
        while let Expr::Binary(e) = attr_target {
            attr_target = &mut e.left;
        }
        attrs.extend(attr_target.replace_attrs(Vec::new()));
        attr_target.replace_attrs(attrs);

        if input.peek(Token![;]) {
            return Ok(Stmt::Semi(e, input.parse()?));
        }

        if allow_nosemi || !expr::requires_terminator(&e) {
            Ok(Stmt::Expr(e))
        } else {
            Err(input.error("expected semicolon"))
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    impl ToTokens for Block {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.stmts);
            });
        }
    }

    impl ToTokens for Stmt {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Stmt::Local(local) => local.to_tokens(tokens),
                Stmt::Expr(expr) => expr.to_tokens(tokens),
                Stmt::Semi(expr, semi) => {
                    expr.to_tokens(tokens);
                    semi.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for Local {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            expr::printing::outer_attrs_to_tokens(&self.attrs, tokens);
            self.let_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            if let Some((eq_token, init)) = &self.init {
                eq_token.to_tokens(tokens);
                init.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }
}
