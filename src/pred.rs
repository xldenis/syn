use super::*;
// use crate::punctuated::Punctuated;
// #[cfg(feature = "full")]
// use crate::reserved::Reserved;
use proc_macro2::{Span, TokenStream};
// #[cfg(feature = "printing")]
// use quote::IdentFragment;
// #[cfg(feature = "printing")]
// use std::fmt::{self, Display};
// use std::hash::{Hash, Hasher};
#[cfg(feature = "parsing")]
use std::mem;

use crate::token::Paren;

ast_enum_of_structs! {
    pub enum Pred {
        Conj(PredConj),

        Disj(PredDisj),
        Binary(PredBinary),

        Impl(PredImpl),
        Neg(PredNeg),

        Paren(PredParen),
        #[doc(hidden)]
        __Nonexhaustive,
    }
}

ast_struct! {
    pub struct PredConj {
        pub left: Box<Pred>,
        pub conj_token: Token![&&],
        pub right: Box<Pred>,
    }
}

ast_struct! {
    pub struct PredDisj {
        pub left: Box<Pred>,
        pub disj_token: Token![||],
        pub right: Box<Pred>,
    }
}

ast_struct! {
    pub struct PredBinary {
        pub left: Box<Term>,
        pub op: BinOp,
        pub right: Box<Term>,
    }
}

ast_struct! {
    pub struct PredImpl {
        pub hyp: Box<Pred>,
        pub impl_token: Token![==>],
        pub cons: Box<Pred>,
    }
}

ast_struct! {
    pub struct PredNeg {

    }
}

ast_struct! {
    pub struct PredParen {
        pub paren_token: token::Paren,
        pub pred: Box<Pred>,
    }
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use super::*;
    use crate::parse::{Parse, ParseStream, Result};

    impl Parse for Pred {
        fn parse(input: ParseStream) -> Result<Self> {
            let lhs = parse_atom_pred(input)?;
            parse_pred(input, lhs)
        }
    }

    fn parse_pred(
        input: ParseStream,
        mut lhs: Pred,
    ) -> Result<Pred> {
        loop {
            if input.peek(Token![&&]) {
                let conj_token = input.parse()?;

                let rhs = parse_atom_pred(input)?;

                lhs = Pred::Conj(PredConj { left: Box::new(lhs), conj_token, right: Box::new(rhs) });
            } if input.peek(Token![==>]) {
                let impl_token = input.parse()?;

                let cons = parse_atom_pred(input)?;
                let cons = parse_pred(input, cons)?;

                lhs = Pred::Impl(PredImpl { hyp: Box::new(lhs), impl_token, cons: Box::new(cons)})
            }
            else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_atom_pred(input: ParseStream) -> Result<Pred> {
        if input.peek(token::Paren) {
            let content;
            let paren_token = parenthesized!(content in input);
            let pred = content.parse()?;
            Ok(Pred::Paren(PredParen { paren_token, pred: Box::new(pred) }))
        } else {
            let TermBinary { left, op, right } = TermBinary::parse(input)?;
            Ok(Pred::Binary(PredBinary { left, op, right }))
        }
    }
}

#[cfg(feature = "printing")]
pub(crate) mod printing {
    use super::*;
    use quote::ToTokens;

    impl ToTokens for PredConj {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.left.to_tokens(tokens);
            self.conj_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }

    impl ToTokens for PredDisj {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.left.to_tokens(tokens);
            self.disj_token.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }
    impl ToTokens for PredBinary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.left.to_tokens(tokens);
            self.op.to_tokens(tokens);
            self.right.to_tokens(tokens);
        }
    }
    impl ToTokens for PredImpl {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.hyp.to_tokens(tokens);
            self.impl_token.to_tokens(tokens);
            self.cons.to_tokens(tokens);
        }
    }
    impl ToTokens for PredNeg {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            todo!()
        }
    }

    impl ToTokens for PredParen {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.pred.to_tokens(tokens);
            });
        }
    }
}
