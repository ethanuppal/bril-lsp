// Copyright (C) 2024 Ethan Uppal.
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt;

use crate::loc::Loc;

pub struct Program<'a> {
    pub items: Vec<TopLevelItem<'a>>,
}

impl<'a> Program<'a> {
    pub fn imports(&self) -> impl Iterator<Item = &Loc<Import<'a>>> {
        self.items.iter().filter_map(|item| match item {
            TopLevelItem::Import(import) => Some(import),
            _ => None,
        })
    }

    pub fn functions(&self) -> impl Iterator<Item = &Loc<Function<'a>>> {
        self.items.iter().filter_map(|item| match item {
            TopLevelItem::Function(function) => Some(function),
            _ => None,
        })
    }
}

pub enum TopLevelItem<'a> {
    Import(Loc<Import<'a>>),
    Function(Loc<Function<'a>>),
    Comment(Loc<&'a str>),
    Newline(Loc<()>),
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
        comment: Option<Loc<&'a str>>,
    },
    Instruction {
        inner: Loc<Instruction<'a>>,
        comment: Option<Loc<&'a str>>,
    },
    Comment(Loc<&'a str>),
    EmptyLine(Loc<()>),
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
            (Type::Ptr(inner), Type::Ptr(inner2)) => inner.is_same_type_as(inner2),
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

    Alloc(Loc<&'a str>),
    Load(Loc<&'a str>),
    PtrAdd(Loc<&'a str>, Loc<&'a str>),

    Ceq(Loc<&'a str>, Loc<&'a str>),
    Clt(Loc<&'a str>, Loc<&'a str>),
    Cgt(Loc<&'a str>, Loc<&'a str>),
    Cle(Loc<&'a str>, Loc<&'a str>),
    Cge(Loc<&'a str>, Loc<&'a str>),
    Char2Int(Loc<&'a str>),
    Int2Char(Loc<&'a str>),
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

    Store(Loc<&'a str>, Loc<&'a str>),
    Free(Loc<&'a str>),
}

pub struct EffectOperation<'a> {
    pub op: Loc<EffectOperationOp<'a>>,
    pub semi_token: Loc<()>,
}
