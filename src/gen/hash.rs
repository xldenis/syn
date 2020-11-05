// This file is @generated by syn-internal-codegen.
// It is not intended for manual editing.

#[cfg(any(feature = "derive", feature = "full"))]
use crate::tt::TokenStreamHelper;
use crate::*;
use std::hash::{Hash, Hasher};
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Abi {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.name.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for AngleBracketedGenericArguments {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.colon2_token.hash(state);
        self.args.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for Arm {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.pat.hash(state);
        self.guard.hash(state);
        self.body.hash(state);
        self.comma.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for AttrStyle {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            AttrStyle::Outer => {
                state.write_u8(0u8);
            }
            AttrStyle::Inner(_) => {
                state.write_u8(1u8);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Attribute {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.style.hash(state);
        self.path.hash(state);
        TokenStreamHelper(&self.tokens).hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for BareFnArg {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.name.hash(state);
        self.ty.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for BinOp {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            BinOp::Add(_) => {
                state.write_u8(0u8);
            }
            BinOp::Sub(_) => {
                state.write_u8(1u8);
            }
            BinOp::Mul(_) => {
                state.write_u8(2u8);
            }
            BinOp::Div(_) => {
                state.write_u8(3u8);
            }
            BinOp::Rem(_) => {
                state.write_u8(4u8);
            }
            BinOp::And(_) => {
                state.write_u8(5u8);
            }
            BinOp::Or(_) => {
                state.write_u8(6u8);
            }
            BinOp::BitXor(_) => {
                state.write_u8(7u8);
            }
            BinOp::BitAnd(_) => {
                state.write_u8(8u8);
            }
            BinOp::BitOr(_) => {
                state.write_u8(9u8);
            }
            BinOp::Shl(_) => {
                state.write_u8(10u8);
            }
            BinOp::Shr(_) => {
                state.write_u8(11u8);
            }
            BinOp::Eq(_) => {
                state.write_u8(12u8);
            }
            BinOp::Lt(_) => {
                state.write_u8(13u8);
            }
            BinOp::Le(_) => {
                state.write_u8(14u8);
            }
            BinOp::Ne(_) => {
                state.write_u8(15u8);
            }
            BinOp::Ge(_) => {
                state.write_u8(16u8);
            }
            BinOp::Gt(_) => {
                state.write_u8(17u8);
            }
            BinOp::AddEq(_) => {
                state.write_u8(18u8);
            }
            BinOp::SubEq(_) => {
                state.write_u8(19u8);
            }
            BinOp::MulEq(_) => {
                state.write_u8(20u8);
            }
            BinOp::DivEq(_) => {
                state.write_u8(21u8);
            }
            BinOp::RemEq(_) => {
                state.write_u8(22u8);
            }
            BinOp::BitXorEq(_) => {
                state.write_u8(23u8);
            }
            BinOp::BitAndEq(_) => {
                state.write_u8(24u8);
            }
            BinOp::BitOrEq(_) => {
                state.write_u8(25u8);
            }
            BinOp::ShlEq(_) => {
                state.write_u8(26u8);
            }
            BinOp::ShrEq(_) => {
                state.write_u8(27u8);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Binding {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.ident.hash(state);
        self.ty.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for Block {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.stmts.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for BoundLifetimes {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lifetimes.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ConstParam {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.ident.hash(state);
        self.ty.hash(state);
        self.eq_token.hash(state);
        self.default.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Constraint {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.ident.hash(state);
        self.bounds.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Expr {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            #[cfg(feature = "full")]
            Expr::Array(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Expr::Binary(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Block(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
            Expr::Call(v0) => {
                state.write_u8(3u8);
                v0.hash(state);
            }
            Expr::Cast(v0) => {
                state.write_u8(4u8);
                v0.hash(state);
            }
            Expr::Field(v0) => {
                state.write_u8(5u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Group(v0) => {
                state.write_u8(6u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::If(v0) => {
                state.write_u8(7u8);
                v0.hash(state);
            }
            Expr::Index(v0) => {
                state.write_u8(8u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Let(v0) => {
                state.write_u8(9u8);
                v0.hash(state);
            }
            Expr::Lit(v0) => {
                state.write_u8(10u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Match(v0) => {
                state.write_u8(11u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::MethodCall(v0) => {
                state.write_u8(12u8);
                v0.hash(state);
            }
            Expr::Paren(v0) => {
                state.write_u8(13u8);
                v0.hash(state);
            }
            Expr::Path(v0) => {
                state.write_u8(14u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Range(v0) => {
                state.write_u8(15u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Reference(v0) => {
                state.write_u8(16u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Repeat(v0) => {
                state.write_u8(17u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Return(v0) => {
                state.write_u8(18u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Struct(v0) => {
                state.write_u8(19u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Tuple(v0) => {
                state.write_u8(20u8);
                v0.hash(state);
            }
            #[cfg(feature = "full")]
            Expr::Type(v0) => {
                state.write_u8(21u8);
                v0.hash(state);
            }
            Expr::Unary(v0) => {
                state.write_u8(22u8);
                v0.hash(state);
            }
            Expr::Verbatim(v0) => {
                state.write_u8(23u8);
                TokenStreamHelper(v0).hash(state);
            }
            _ => unreachable!(),
        }
    }
}
#[cfg(feature = "full")]
impl Hash for ExprArray {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.elems.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprBinary {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.left.hash(state);
        self.op.hash(state);
        self.right.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprBlock {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.label.hash(state);
        self.block.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprCall {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.func.hash(state);
        self.args.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprCast {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
        self.ty.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprField {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.base.hash(state);
        self.member.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprGroup {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprIf {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.cond.hash(state);
        self.then_branch.hash(state);
        self.else_branch.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprIndex {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
        self.index.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprLet {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.pat.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprLit {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.lit.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprMatch {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
        self.arms.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprMethodCall {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.receiver.hash(state);
        self.method.hash(state);
        self.turbofish.hash(state);
        self.args.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprParen {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprPath {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.qself.hash(state);
        self.path.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprRange {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.from.hash(state);
        self.limits.hash(state);
        self.to.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprReference {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.mutability.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprRepeat {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
        self.len.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprReturn {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprStruct {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.path.hash(state);
        self.fields.hash(state);
        self.dot2_token.hash(state);
        self.rest.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprTuple {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.elems.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for ExprType {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
        self.ty.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ExprUnary {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.op.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for FieldPat {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.member.hash(state);
        self.colon_token.hash(state);
        self.pat.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for FieldValue {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.member.hash(state);
        self.colon_token.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for GenericArgument {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            GenericArgument::Lifetime(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            GenericArgument::Type(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            GenericArgument::Binding(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
            GenericArgument::Constraint(v0) => {
                state.write_u8(3u8);
                v0.hash(state);
            }
            GenericArgument::Const(v0) => {
                state.write_u8(4u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(feature = "full")]
impl Hash for GenericMethodArgument {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            GenericMethodArgument::Type(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            GenericMethodArgument::Const(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for GenericParam {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            GenericParam::Type(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            GenericParam::Lifetime(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            GenericParam::Const(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Generics {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lt_token.hash(state);
        self.params.hash(state);
        self.gt_token.hash(state);
        self.where_clause.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for Label {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.name.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for LifetimeDef {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.lifetime.hash(state);
        self.colon_token.hash(state);
        self.bounds.hash(state);
    }
}
impl Hash for Lit {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Lit::Str(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Lit::ByteStr(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            Lit::Byte(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
            Lit::Char(v0) => {
                state.write_u8(3u8);
                v0.hash(state);
            }
            Lit::Int(v0) => {
                state.write_u8(4u8);
                v0.hash(state);
            }
            Lit::Float(v0) => {
                state.write_u8(5u8);
                v0.hash(state);
            }
            Lit::Bool(v0) => {
                state.write_u8(6u8);
                v0.hash(state);
            }
            Lit::Verbatim(v0) => {
                state.write_u8(7u8);
                v0.to_string().hash(state);
            }
        }
    }
}
impl Hash for LitBool {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.value.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for Local {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.pat.hash(state);
        self.init.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Meta {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Meta::Path(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Meta::List(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            Meta::NameValue(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for MetaList {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.path.hash(state);
        self.nested.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for MetaNameValue {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.path.hash(state);
        self.lit.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for MethodTurbofish {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.args.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for NestedMeta {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            NestedMeta::Meta(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            NestedMeta::Lit(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ParenthesizedGenericArguments {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.inputs.hash(state);
        self.output.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for Pat {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Pat::Box(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Pat::Ident(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            Pat::Lit(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
            Pat::Or(v0) => {
                state.write_u8(3u8);
                v0.hash(state);
            }
            Pat::Path(v0) => {
                state.write_u8(4u8);
                v0.hash(state);
            }
            Pat::Range(v0) => {
                state.write_u8(5u8);
                v0.hash(state);
            }
            Pat::Reference(v0) => {
                state.write_u8(6u8);
                v0.hash(state);
            }
            Pat::Rest(v0) => {
                state.write_u8(7u8);
                v0.hash(state);
            }
            Pat::Slice(v0) => {
                state.write_u8(8u8);
                v0.hash(state);
            }
            Pat::Struct(v0) => {
                state.write_u8(9u8);
                v0.hash(state);
            }
            Pat::Tuple(v0) => {
                state.write_u8(10u8);
                v0.hash(state);
            }
            Pat::TupleStruct(v0) => {
                state.write_u8(11u8);
                v0.hash(state);
            }
            Pat::Type(v0) => {
                state.write_u8(12u8);
                v0.hash(state);
            }
            Pat::Verbatim(v0) => {
                state.write_u8(13u8);
                TokenStreamHelper(v0).hash(state);
            }
            Pat::Wild(v0) => {
                state.write_u8(14u8);
                v0.hash(state);
            }
            _ => unreachable!(),
        }
    }
}
#[cfg(feature = "full")]
impl Hash for PatBox {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.pat.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatIdent {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.by_ref.hash(state);
        self.mutability.hash(state);
        self.ident.hash(state);
        self.subpat.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatLit {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.expr.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatOr {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.leading_vert.hash(state);
        self.cases.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatPath {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.qself.hash(state);
        self.path.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatRange {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.lo.hash(state);
        self.limits.hash(state);
        self.hi.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatReference {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.mutability.hash(state);
        self.pat.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatRest {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatSlice {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.elems.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatStruct {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.path.hash(state);
        self.fields.hash(state);
        self.dot2_token.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatTuple {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.elems.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatTupleStruct {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.path.hash(state);
        self.pat.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatType {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.pat.hash(state);
        self.ty.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for PatWild {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Path {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.leading_colon.hash(state);
        self.segments.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for PathArguments {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            PathArguments::None => {
                state.write_u8(0u8);
            }
            PathArguments::AngleBracketed(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            PathArguments::Parenthesized(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for PathSegment {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.ident.hash(state);
        self.arguments.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for PredicateEq {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lhs_ty.hash(state);
        self.rhs_ty.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for PredicateLifetime {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lifetime.hash(state);
        self.bounds.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for PredicateType {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lifetimes.hash(state);
        self.bounded_ty.hash(state);
        self.bounds.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for QSelf {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.ty.hash(state);
        self.position.hash(state);
        self.as_token.hash(state);
    }
}
#[cfg(feature = "full")]
impl Hash for RangeLimits {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            RangeLimits::HalfOpen(_) => {
                state.write_u8(0u8);
            }
            RangeLimits::Closed(_) => {
                state.write_u8(1u8);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for ReturnType {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            ReturnType::Default => {
                state.write_u8(0u8);
            }
            ReturnType::Type(_, v1) => {
                state.write_u8(1u8);
                v1.hash(state);
            }
        }
    }
}
#[cfg(feature = "full")]
impl Hash for Stmt {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Stmt::Local(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Stmt::Expr(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            Stmt::Semi(v0, _) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TraitBound {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.paren_token.hash(state);
        self.modifier.hash(state);
        self.lifetimes.hash(state);
        self.path.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TraitBoundModifier {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            TraitBoundModifier::None => {
                state.write_u8(0u8);
            }
            TraitBoundModifier::Maybe(_) => {
                state.write_u8(1u8);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Type {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Type::Array(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            Type::BareFn(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            Type::Group(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
            Type::ImplTrait(v0) => {
                state.write_u8(3u8);
                v0.hash(state);
            }
            Type::Infer(v0) => {
                state.write_u8(4u8);
                v0.hash(state);
            }
            Type::Never(v0) => {
                state.write_u8(5u8);
                v0.hash(state);
            }
            Type::Paren(v0) => {
                state.write_u8(6u8);
                v0.hash(state);
            }
            Type::Path(v0) => {
                state.write_u8(7u8);
                v0.hash(state);
            }
            Type::Ptr(v0) => {
                state.write_u8(8u8);
                v0.hash(state);
            }
            Type::Reference(v0) => {
                state.write_u8(9u8);
                v0.hash(state);
            }
            Type::Slice(v0) => {
                state.write_u8(10u8);
                v0.hash(state);
            }
            Type::TraitObject(v0) => {
                state.write_u8(11u8);
                v0.hash(state);
            }
            Type::Tuple(v0) => {
                state.write_u8(12u8);
                v0.hash(state);
            }
            Type::Verbatim(v0) => {
                state.write_u8(13u8);
                TokenStreamHelper(v0).hash(state);
            }
            _ => unreachable!(),
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeArray {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.elem.hash(state);
        self.len.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeBareFn {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lifetimes.hash(state);
        self.unsafety.hash(state);
        self.abi.hash(state);
        self.inputs.hash(state);
        self.variadic.hash(state);
        self.output.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeGroup {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.elem.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeImplTrait {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.bounds.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeInfer {
    fn hash<H>(&self, _state: &mut H)
    where
        H: Hasher,
    {
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeNever {
    fn hash<H>(&self, _state: &mut H)
    where
        H: Hasher,
    {
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeParam {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.ident.hash(state);
        self.colon_token.hash(state);
        self.bounds.hash(state);
        self.eq_token.hash(state);
        self.default.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeParamBound {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            TypeParamBound::Trait(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            TypeParamBound::Lifetime(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeParen {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.elem.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypePath {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.qself.hash(state);
        self.path.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypePtr {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.const_token.hash(state);
        self.mutability.hash(state);
        self.elem.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeReference {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.lifetime.hash(state);
        self.mutability.hash(state);
        self.elem.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeSlice {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.elem.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeTraitObject {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.dyn_token.hash(state);
        self.bounds.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for TypeTuple {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.elems.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for UnOp {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            UnOp::Deref(_) => {
                state.write_u8(0u8);
            }
            UnOp::Not(_) => {
                state.write_u8(1u8);
            }
            UnOp::Neg(_) => {
                state.write_u8(2u8);
            }
        }
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for Variadic {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for WhereClause {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.predicates.hash(state);
    }
}
#[cfg(any(feature = "derive", feature = "full"))]
impl Hash for WherePredicate {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            WherePredicate::Type(v0) => {
                state.write_u8(0u8);
                v0.hash(state);
            }
            WherePredicate::Lifetime(v0) => {
                state.write_u8(1u8);
                v0.hash(state);
            }
            WherePredicate::Eq(v0) => {
                state.write_u8(2u8);
                v0.hash(state);
            }
        }
    }
}
