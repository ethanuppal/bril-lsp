use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
};

use crate::{
    ast::{self, Type},
    loc::Loc,
    parser::Diagnostic,
};

pub fn create_function_context(
    functions: &[Loc<ast::Function>],
) -> HashMap<String, (Vec<Type>, Option<Type>)> {
    let mut context = HashMap::new();
    for function in functions {
        let mut parameter_types = vec![];
        let mut return_type = None;

        for (_, type_annotation) in &function.parameters {
            parameter_types.push(type_annotation.ty.inner.clone());
        }

        if let Some(return_type_annotation) = &function.return_type {
            return_type = Some(return_type_annotation.ty.inner.clone());
        }

        context
            .insert(function.name.to_string(), (parameter_types, return_type));
    }
    context
}

pub fn type_infer_function(
    context: &HashMap<String, (Vec<Type>, Option<Type>)>,
    function: &ast::Function,
) -> Result<BTreeMap<String, Type>, Diagnostic> {
    let symbols = RefCell::new(HashMap::new());
    let env = RefCell::new(BTreeMap::new());
    let mut return_type = None;

    for (parameter, type_annotation) in &function.parameters {
        if env.borrow().contains_key(&parameter.to_string()) {
            let symbols = symbols.borrow();
            let previous = symbols
                .get(&parameter.to_string())
                .expect("Should be in symbols map");
            return Err(Diagnostic::new("Duplicate parameter name", parameter)
                .label("Previous definition here", previous));
        }
        symbols
            .borrow_mut()
            .insert(parameter.to_string(), parameter.clone());
        env.borrow_mut()
            .insert(parameter.to_string(), type_annotation.ty.inner.clone());
    }

    if let Some(return_type_annotation) = &function.return_type {
        return_type = Some(return_type_annotation.ty.inner.clone());
    }

    let get_signature =
        |name: &Loc<&str>| -> Result<(Vec<Type>, Option<Type>), Diagnostic> {
            if let Some(signature) = context.get(&name.to_string()) {
                Ok(signature.clone())
            } else {
                Err(Diagnostic::new("Called undefined function", name))
            }
        };

    let ensure =
        |op: &str, arg: &Loc<&str>, ty: Type| -> Result<(), Diagnostic> {
            if let Some(arg_type) = env.borrow().get(&arg.to_string()) {
                if arg_type.is_same_type_as(&ty) {
                    Ok(())
                } else {
                    let symbols = symbols.borrow();
                    let original = symbols
                        .get(&arg.to_string())
                        .expect("type exists so symbol does too");
                    Err(Diagnostic::new(
                        format!(
                            "Expected argument of type {} in {} instruction",
                            ty, op
                        ),
                        arg,
                    )
                    .label(
                        format!("Type inferred here to be {}", arg_type),
                        original,
                    ))
                }
            } else {
                Err(Diagnostic::new(
                    format!("Undefined variable in this {} instruction", op),
                    arg,
                ))
            }
        };

    for code in &function.body {
        if let ast::FunctionCode::Instruction(instruction) = &**code {
            match &**instruction {
                ast::Instruction::Constant(constant) => {
                    let mut inferred_type = match &*constant.value {
                        ast::ConstantValue::IntegerLiteral(_) => ast::Type::Int,
                        ast::ConstantValue::BooleanLiteral(_) => {
                            ast::Type::Bool
                        }
                        ast::ConstantValue::FloatLiteral(_) => ast::Type::Float,
                        ast::ConstantValue::CharacterLiteral(_) => {
                            ast::Type::Char
                        }
                    };

                    if let Some(annotation) = &constant.type_annotation {
                        if !annotation.ty.inner.is_same_type_as(&inferred_type)
                        {
                            if annotation.ty.is_same_type_as(&Type::Float)
                                && inferred_type.is_same_type_as(&Type::Int)
                            {
                                // don't think this should be valid but some
                                // programs rely on it
                                inferred_type = Type::Float;
                            } else {
                                return Err(Diagnostic::new(format!("Inferred type {inferred_type} did not match type annotation"), annotation).label("Type inferred here", &constant.value));
                            }
                        }
                    }

                    symbols.borrow_mut().insert(
                        constant.name.to_string(),
                        constant.name.clone(),
                    );
                    env.borrow_mut()
                        .insert(constant.name.to_string(), inferred_type);
                }
                ast::Instruction::ValueOperation(value_operation) => {
                    let inferred_type = match &*value_operation.op {
                        ast::ValueOperationOp::Add(lhs, rhs) => {
                            ensure("add", lhs, Type::Int)?;
                            ensure("add", rhs, Type::Int)?;
                            Type::Int
                        }
                        ast::ValueOperationOp::Mul(lhs, rhs) => {
                            ensure("mul", lhs, Type::Int)?;
                            ensure("mul", rhs, Type::Int)?;
                            Type::Int
                        }
                        ast::ValueOperationOp::Sub(lhs, rhs) => {
                            ensure("sub", lhs, Type::Int)?;
                            ensure("sub", rhs, Type::Int)?;
                            Type::Int
                        }
                        ast::ValueOperationOp::Div(lhs, rhs) => {
                            ensure("div", lhs, Type::Int)?;
                            ensure("div", rhs, Type::Int)?;
                            Type::Int
                        }
                        ast::ValueOperationOp::Eq(lhs, rhs) => {
                            ensure("eq", lhs, Type::Int)?;
                            ensure("eq", rhs, Type::Int)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Lt(lhs, rhs) => {
                            ensure("lt", lhs, Type::Int)?;
                            ensure("lt", rhs, Type::Int)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Gt(lhs, rhs) => {
                            ensure("gt", lhs, Type::Int)?;
                            ensure("gt", rhs, Type::Int)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Le(lhs, rhs) => {
                            ensure("le", lhs, Type::Int)?;
                            ensure("le", rhs, Type::Int)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Ge(lhs, rhs) => {
                            ensure("ge", lhs, Type::Int)?;
                            ensure("ge", rhs, Type::Int)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Not(value) => {
                            ensure("not", value, Type::Bool)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::And(lhs, rhs) => {
                            ensure("and", lhs, Type::Bool)?;
                            ensure("and", rhs, Type::Bool)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Or(lhs, rhs) => {
                            ensure("or", lhs, Type::Bool)?;
                            ensure("or", rhs, Type::Bool)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Call(name, args) => {
                            let signature = get_signature(name)?;
                            if args.len() != signature.0.len() {
                                return Err(Diagnostic::new(format!("Called function takes {} argument(s) but was passed {}", signature.0.len(), args.len()), name));
                            }
                            for (i, parameter_type) in
                                signature.0.iter().enumerate()
                            {
                                ensure(
                                    "call",
                                    &args[i],
                                    parameter_type.clone(),
                                )?;
                            }
                            signature.1.clone().ok_or(Diagnostic::new("Called function has no return type, but call used as value operation", name))?
                        }
                        ast::ValueOperationOp::Id(value) => {
                            let Some(ty) =
                                env.borrow().get(&value.to_string()).cloned()
                            else {
                                return Err(Diagnostic::new(
                                    "Undefined variable in id instruction",
                                    value,
                                ));
                            };
                            ty
                        }
                        ast::ValueOperationOp::Fadd(lhs, rhs) => {
                            ensure("fadd", lhs, Type::Float)?;
                            ensure("fadd", rhs, Type::Float)?;
                            Type::Float
                        }
                        ast::ValueOperationOp::Fmul(lhs, rhs) => {
                            ensure("fmul", lhs, Type::Float)?;
                            ensure("fmul", rhs, Type::Float)?;
                            Type::Float
                        }
                        ast::ValueOperationOp::Fsub(lhs, rhs) => {
                            ensure("fsub", lhs, Type::Float)?;
                            ensure("fsub", rhs, Type::Float)?;
                            Type::Float
                        }
                        ast::ValueOperationOp::Fdiv(lhs, rhs) => {
                            ensure("fdiv", lhs, Type::Float)?;
                            ensure("fdiv", rhs, Type::Float)?;
                            Type::Float
                        }
                        ast::ValueOperationOp::Feq(lhs, rhs) => {
                            ensure("feq", lhs, Type::Float)?;
                            ensure("feq", rhs, Type::Float)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Flt(lhs, rhs) => {
                            ensure("flt", lhs, Type::Float)?;
                            ensure("flt", rhs, Type::Float)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Fle(lhs, rhs) => {
                            ensure("fle", lhs, Type::Float)?;
                            ensure("fle", rhs, Type::Float)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Fgt(lhs, rhs) => {
                            ensure("fgt", lhs, Type::Float)?;
                            ensure("fgt", rhs, Type::Float)?;
                            Type::Bool
                        }
                        ast::ValueOperationOp::Fge(lhs, rhs) => {
                            ensure("fge", lhs, Type::Float)?;
                            ensure("fge", rhs, Type::Float)?;
                            Type::Bool
                        }
                    };

                    if let Some(annotation) = &value_operation.type_annotation {
                        if !annotation.ty.inner.is_same_type_as(&inferred_type)
                        {
                            return Err(Diagnostic::new(format!("Inferred type {inferred_type} did not match type annotation"), annotation).label("Type inferred here", &value_operation.op));
                        }
                    }

                    symbols.borrow_mut().insert(
                        value_operation.name.to_string(),
                        value_operation.name.clone(),
                    );
                    env.borrow_mut().insert(
                        value_operation.name.to_string(),
                        inferred_type,
                    );
                }
                ast::Instruction::EffectOperation(effect_operation) => {
                    match &*effect_operation.op {
                        ast::EffectOperationOp::Jmp(_) => {}
                        ast::EffectOperationOp::Br(condition, _, _) => {
                            ensure("br", condition, Type::Bool)?;
                        }
                        ast::EffectOperationOp::Call(name, args) => {
                            let signature = get_signature(name)?;
                            if args.len() != signature.0.len() {
                                return Err(Diagnostic::new(format!("Called function takes {} argument(s) but was passed {}", signature.0.len(), args.len()), name));
                            }
                            for (i, parameter_type) in
                                signature.0.iter().enumerate()
                            {
                                ensure(
                                    "call",
                                    &args[i],
                                    parameter_type.clone(),
                                )?;
                            }
                            if let Some(return_type) = &signature.1 {
                                return Err(Diagnostic::new(format!("Called function returns {}, but call used as effect operation", return_type), name));
                            }
                        }
                        ast::EffectOperationOp::Ret(value) => {
                            if let Some(value) = value {
                                if let Some(return_type) = return_type.clone() {
                                    ensure("ret", value, return_type)?;
                                } else {
                                    return Err(Diagnostic::new("Tried to return value, but function has no return type", value));
                                }
                            } else if let Some(return_type) = return_type {
                                return Err(Diagnostic::new(format!("Tried to return nothing, but function has return type {}", return_type), effect_operation));
                            }
                        }
                        ast::EffectOperationOp::Print(_) => {}
                        ast::EffectOperationOp::Nop => {}
                    }
                }
            }
        }
    }

    Ok(env.take())
}
