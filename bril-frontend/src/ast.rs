// Copyright (C) 2024 Ethan Uppal.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3 of the License only.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <https://www.gnu.org/licenses/>.

use std::fmt;

use crate::loc::Loc;

pub struct Program<'a> {
    pub imports: Vec<Loc<Import<'a>>>,
    pub functions: Vec<Loc<Function<'a>>>,
}

pub struct ImportedFunctionAlias<'a> {
    pub as_token: Loc<()>,
    pub name: Loc<&'a str>,
}

pub struct ImportedFunction<'a> {
    pub name: Loc<&'a str>,
    pub alias: Option<Loc<ImportedFunctionAlias<'a>>>,
}

pub struct Import<'a> {
    pub from_token: Loc<()>,
    pub path: Loc<&'a str>,
    pub import_token: Loc<()>,
    pub imported_functions: Vec<Loc<ImportedFunction<'a>>>,
}

pub struct Function<'a> {
    pub name: Loc<&'a str>,
    pub parameters: Vec<(Loc<&'a str>, Loc<TypeAnnotation>)>,
    pub return_type: Option<Loc<TypeAnnotation>>,
    pub body: Vec<Loc<FunctionCode<'a>>>,
}

pub enum FunctionCode<'a> {
    Label {
        label: Loc<Label<'a>>,
        colon_token: Loc<()>,
    },
    Instruction(Loc<Instruction<'a>>),
}

pub struct Label<'a> {
    pub name: Loc<&'a str>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Float,
    Char,
    Ptr(Box<Loc<Type>>),
}

impl Type {
    pub fn is_same_type_as(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Float, Type::Float)
            | (Type::Char, Type::Char) => true,
            (Type::Ptr(inner), Type::Ptr(inner2)) => {
                inner.is_same_type_as(inner2)
            }
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => "int".fmt(f),
            Type::Bool => "bool".fmt(f),
            Type::Float => "float".fmt(f),
            Type::Char => "char".fmt(f),
            Type::Ptr(inner) => write!(f, "ptr<{}>", inner),
        }
    }
}

pub struct TypeAnnotation {
    pub colon_token: Loc<()>,
    pub ty: Loc<Type>,
}

pub enum Instruction<'a> {
    Constant(Loc<Constant<'a>>),
    ValueOperation(Loc<ValueOperation<'a>>),
    EffectOperation(Loc<EffectOperation<'a>>),
}

pub enum ConstantValue {
    IntegerLiteral(Loc<i64>),
    BooleanLiteral(Loc<bool>),
    FloatLiteral(Loc<f64>),
    CharacterLiteral(Loc<char>),
}

pub struct Constant<'a> {
    pub name: Loc<&'a str>,
    pub type_annotation: Option<Loc<TypeAnnotation>>,
    pub equals_token: Loc<()>,
    pub const_token: Loc<()>,
    pub value: Loc<ConstantValue>,
    pub semi_token: Loc<()>,
}

pub enum ValueOperationOp<'a> {
    Add(Loc<&'a str>, Loc<&'a str>),
    Mul(Loc<&'a str>, Loc<&'a str>),
    Sub(Loc<&'a str>, Loc<&'a str>),
    Div(Loc<&'a str>, Loc<&'a str>),

    Eq(Loc<&'a str>, Loc<&'a str>),
    Lt(Loc<&'a str>, Loc<&'a str>),
    Gt(Loc<&'a str>, Loc<&'a str>),
    Le(Loc<&'a str>, Loc<&'a str>),
    Ge(Loc<&'a str>, Loc<&'a str>),

    Not(Loc<&'a str>),
    And(Loc<&'a str>, Loc<&'a str>),
    Or(Loc<&'a str>, Loc<&'a str>),

    /// Value-operation version.
    Call(Loc<&'a str>, Vec<Loc<&'a str>>),
    Id(Loc<&'a str>),

    Fadd(Loc<&'a str>, Loc<&'a str>),
    Fmul(Loc<&'a str>, Loc<&'a str>),
    Fsub(Loc<&'a str>, Loc<&'a str>),
    Fdiv(Loc<&'a str>, Loc<&'a str>),
    Feq(Loc<&'a str>, Loc<&'a str>),
    Flt(Loc<&'a str>, Loc<&'a str>),
    Fle(Loc<&'a str>, Loc<&'a str>),
    Fgt(Loc<&'a str>, Loc<&'a str>),
    Fge(Loc<&'a str>, Loc<&'a str>),
}

pub struct ValueOperation<'a> {
    pub name: Loc<&'a str>,
    pub type_annotation: Option<Loc<TypeAnnotation>>,
    pub equals_token: Loc<()>,
    pub op: Loc<ValueOperationOp<'a>>,
    pub semi_token: Loc<()>,
}

pub enum EffectOperationOp<'a> {
    Jmp(Loc<Label<'a>>),
    Br(Loc<&'a str>, Loc<Label<'a>>, Loc<Label<'a>>),
    /// Effect-operation version.
    Call(Loc<&'a str>, Vec<Loc<&'a str>>),
    Ret(Option<Loc<&'a str>>),

    Print(Vec<Loc<&'a str>>),
    Nop,
}

pub struct EffectOperation<'a> {
    pub op: Loc<EffectOperationOp<'a>>,
    pub semi_token: Loc<()>,
}
