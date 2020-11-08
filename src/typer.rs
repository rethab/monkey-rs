use crate::ast;
use crate::tast::TypeVariable::*;
use crate::tast::{self, *};
use std::collections::HashMap;

pub fn infer_types(program: ast::Program) -> Result<Program, String> {
    let mut equations = Vec::new();
    let mut assignments = assign_types_program(program, &mut equations)?;
    println!("Assignments: {:?}", assignments);
    println!("Equations: {:?}", equations);
    let solutions = unify(equations)?;
    println!("Solutions: {:?}", solutions);
    set_types_program(&mut assignments, &solutions);
    Ok(assignments)
}

fn assign_types_program(
    program: ast::Program,
    equations: &mut Equations,
) -> Result<Program, String> {
    let mut stmts = Vec::new();
    let mut next_id = TypeId::default();
    let mut ctx = Context::default();
    for stmt in program.0 {
        let (assigned, nxt_id, nxt_ctx) = assign_types_statement(stmt, equations, next_id, ctx)?;
        stmts.push(assigned);
        next_id = nxt_id;
        ctx = nxt_ctx;
    }
    Ok(Program(stmts))
}

fn assign_types_statement(
    stmt: ast::Statement,
    equations: &mut Equations,
    cur_id: TypeId,
    ctx: Context,
) -> Result<(Statement, TypeId, Context), String> {
    use ast::Statement::*;
    match stmt {
        Expression { value, .. } => {
            let (exp, next_id) = assign_types_expression(value, equations, cur_id, ctx.clone())?;
            Ok((Statement::Expression { value: exp }, next_id, ctx))
        }
        Let {
            name, expression, ..
        } => {
            let (exp, next_id) =
                assign_types_expression(*expression, equations, cur_id, ctx.clone())?;
            let (ident_id, next_id) = next_id.next();
            let ident = Identifier {
                tpe: ident_id,
                value: name.value,
            };

            let next_ctx = if let tast::Expression::FunctionLiteral { parameters, .. } = &exp {
                ctx.with_function(ident.clone(), parameters.clone())
            } else {
                ctx
            };

            equations.push(Equation::IsEqual(ident.tpe(), exp.tpe()));

            Ok((
                Statement::Let {
                    name: ident.clone(),
                    expression: Box::new(exp),
                },
                next_id,
                next_ctx.with_identifier(ident),
            ))
        }
        Block { statements, .. } => {
            let (stmts, next_id) =
                assign_types_statements(statements, equations, cur_id, ctx.clone())?;
            Ok((Statement::Block { statements: stmts }, next_id, ctx))
        }
        Return {
            value: expression, ..
        } => {
            let (exp, next_id) =
                assign_types_expression(expression, equations, cur_id, ctx.clone())?;
            Ok((Statement::Return { value: exp }, next_id, ctx))
        }
    }
}

fn assign_types_statements(
    statements: Vec<ast::Statement>,
    equations: &mut Equations,
    cur_id: TypeId,
    ctx: Context,
) -> Result<(Vec<tast::Statement>, TypeId), String> {
    let mut stmts = Vec::new();
    let mut next_id = cur_id;
    let mut sub_ctx = ctx;
    let size = statements.len();
    for (index, stmt) in statements.into_iter().enumerate() {
        let (assigned, nxt_id, next_ctx) =
            assign_types_statement(stmt, equations, next_id, sub_ctx)?;
        if let Statement::Return { .. } = assigned {
            if index < size - 1 {
                return Err("Unreachable statement(s) after return".to_string());
            }
        }
        stmts.push(assigned);
        next_id = nxt_id;
        sub_ctx = next_ctx;
    }
    Ok((stmts, next_id))
}

fn assign_types_expression(
    exp: ast::Expression,
    equations: &mut Equations,
    cur_id: TypeId,
    ctx: Context,
) -> Result<(Expression, TypeId), String> {
    use ast::Expression::*;
    match exp {
        IntLiteral { value, .. } => Ok((Expression::IntLiteral { value }, cur_id)),
        StringLiteral { value, .. } => Ok((Expression::StringLiteral { value }, cur_id)),
        BooleanLiteral { value, .. } => Ok((Expression::BooleanLiteral { value }, cur_id)),
        Identifier(ident) => {
            let tvar = ctx
                .lookup_identifier(&ident)
                .ok_or(format!("Identifier {:?} not found in context", ident))?;

            Ok((
                Expression::Identifier(tast::Identifier {
                    tpe: tvar,
                    value: ident.value,
                }),
                cur_id,
            ))
        }
        ArrayLiteral { values, .. } => {
            let mut vals = Vec::new();
            let mut next_id = cur_id;
            for value in values {
                let (val, nxt_id) =
                    assign_types_expression(value, equations, next_id, ctx.clone())?;
                vals.push(val);
                next_id = nxt_id;
            }

            let (element_type, next_id) = next_id.next();

            let mut prev_elem = element_type.clone();
            for val in vals.iter() {
                equations.push(Equation::IsEqual(prev_elem.clone(), val.tpe()));
                prev_elem = val.tpe();
            }

            let array = Expression::ArrayLiteral {
                tpe: element_type,
                values: vals,
            };

            Ok((array, next_id))
        }
        Prefix { op, rhs, .. } => {
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, equations, cur_id, ctx)?;
            let (id, next_id) = next_id.next();
            let infix = Expression::Prefix {
                tpe: id,
                op,
                rhs: Box::new(rhs_assigned),
            };
            Ok((infix, next_id))
        }
        Infix { op, rhs, lhs, .. } => {
            let (lhs_assigned, next_id) =
                assign_types_expression(*lhs, equations, cur_id, ctx.clone())?;
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, equations, next_id, ctx)?;
            let (infix_type, next) = next_id.next();
            match op.as_str() {
                "-" | "*" | "/" => {
                    equations.push(Equation::IsEqual(infix_type.clone(), Known(Type::Int)));
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                }
                "+" => {
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                    equations.push(Equation::IsEqual(infix_type.clone(), rhs_assigned.tpe()));
                }
                "==" => {
                    equations.push(Equation::IsEqual(infix_type.clone(), Known(Type::Boolean)));
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                }
                other => {
                    return Err(format!("Unhandled infix operator: {}", other));
                }
            }
            let infix = Expression::Infix {
                tpe: infix_type,
                op,
                lhs: Box::new(lhs_assigned),
                rhs: Box::new(rhs_assigned),
            };
            Ok((infix, next))
        }
        If {
            condition: cond,
            consequence: then,
            alternative: mb_alt,
            ..
        } => {
            let (cond_assigned, next_id) =
                assign_types_expression(*cond, equations, cur_id, ctx.clone())?;
            let (then_assigned, next_id, _) =
                assign_types_statement(*then, equations, next_id, ctx.clone())?;
            let (mb_alt_assigned, next_id) = match mb_alt {
                Some(alt) => {
                    let (a_s, next, _) = assign_types_statement(*alt, equations, next_id, ctx)?;
                    (Some(Box::new(a_s)), next)
                }
                None => (None, next_id),
            };
            let (tpe, next) = next_id.next();

            equations.push(Equation::IsEqual(cond_assigned.tpe(), Known(Type::Boolean)));
            equations.push(Equation::IsEqual(tpe.clone(), then_assigned.tpe()));

            if let Some(alt_assigned) = mb_alt_assigned.as_ref() {
                equations.push(Equation::IsEqual(tpe.clone(), alt_assigned.tpe()));
                equations.push(Equation::IsEqual(alt_assigned.tpe(), then_assigned.tpe()));
            }

            let if_exp = Expression::If {
                tpe,
                condition: Box::new(cond_assigned),
                consequence: Box::new(then_assigned),
                alternative: mb_alt_assigned,
            };
            Ok((if_exp, next))
        }
        FunctionLiteral {
            parameters, body, ..
        } => {
            let (parameters, body, next_id) =
                assign_types_function_literal(parameters, equations, *body, cur_id, ctx)?;
            let (function_id, next_id) = next_id.next();
            equations.push(Equation::IsEqual(function_id.clone(), body.tpe()));
            let function = Expression::FunctionLiteral {
                return_type: function_id,
                parameters,
                body: Box::new(body),
            };
            Ok((function, next_id))
        }
        Index {
            container, index, ..
        } => {
            let (cont, next_id) =
                assign_types_expression(*container, equations, cur_id, ctx.clone())?;
            let (idx, next_id) = assign_types_expression(*index, equations, next_id, ctx)?;
            let (array_value_type, next_id) = next_id.next();

            equations.push(Equation::IsEqual(cont.tpe(), array_value_type.clone()));
            equations.push(Equation::IsEqual(idx.tpe(), Known(Type::Int)));

            let index = Expression::Index {
                tpe: array_value_type,
                container: Box::new(cont),
                index: Box::new(idx),
            };

            Ok((index, next_id))
        }
        Call {
            function,
            arguments,
            ..
        } => {
            let (call_type, mut next_id) = cur_id.next();

            let mut args = Vec::new();
            for argument in arguments {
                let (arg, nxt_id) =
                    assign_types_expression(argument, equations, next_id, ctx.clone())?;
                args.push(arg);
                next_id = nxt_id;
            }

            let (func, next_id) = match function {
                ast::Function::Identifier(identifier) => {
                    let (mut identifiers, next_id) =
                        assign_types_identifiers(vec![identifier].to_vec(), next_id);
                    let identifier = identifiers.remove(0);
                    equations.push(Equation::IsEqual(identifier.tpe(), call_type.clone()));

                    if let Some((parameters, return_type)) = ctx.lookup_function(&identifier) {
                        if args.len() != parameters.len() {
                            return Err(format!(
                                "Function {} takes {} parameters, but {} given",
                                identifier.value,
                                args.len(),
                                parameters.len()
                            ));
                        }
                        equations.push(Equation::IsEqual(call_type.clone(), return_type));
                        for (param, arg) in parameters.iter().zip(args.iter()) {
                            equations.push(Equation::IsEqual(param.tpe(), arg.tpe()));
                        }
                        (Function::Identifier(identifier), next_id)
                    } else {
                        return Err(format!("Function {} not found", identifier.value));
                    }
                }
                ast::Function::Literal {
                    parameters, body, ..
                } => {
                    let (parameters, body, next_id) =
                        assign_types_function_literal(parameters, equations, *body, next_id, ctx)?;
                    let (function_type, next_id) = next_id.next();
                    equations.push(Equation::IsEqual(function_type.clone(), call_type.clone()));
                    equations.push(Equation::IsEqual(function_type.clone(), body.tpe()));
                    equations.push(Equation::IsEqual(body.tpe(), call_type.clone()));
                    if args.len() != parameters.len() {
                        return Err(format!(
                            "Function <anonymous> takes {} parameters, but {} given",
                            args.len(),
                            parameters.len()
                        ));
                    }
                    for (param, arg) in parameters.iter().zip(args.iter()) {
                        equations.push(Equation::IsEqual(param.tpe(), arg.tpe()));
                    }
                    let func = Function::Literal {
                        return_type: function_type,
                        parameters,
                        body: Box::new(body),
                    };
                    (func, next_id)
                }
            };

            let call = Expression::Call {
                tpe: call_type,
                function: func,
                arguments: args,
            };
            Ok((call, next_id))
        }
        other => panic!("assign_types_expression: Unhandled expression: {:?}", other),
    }
}

fn assign_types_function_literal(
    parameters: Vec<ast::Identifier>,
    equations: &mut Equations,
    body: ast::Statement,
    cur_id: TypeId,
    ctx: Context,
) -> Result<(Vec<Identifier>, Statement, TypeId), String> {
    let (params, next_id) = assign_types_identifiers(parameters, cur_id);
    let statements = vec![body].to_vec();
    let sub_ctx = ctx.with_all(params.clone());
    let (mut stmts, next_id) = assign_types_statements(statements, equations, next_id, sub_ctx)?;

    Ok((params, stmts.remove(0), next_id))
}

fn assign_types_identifiers(
    identifiers: Vec<ast::Identifier>,
    cur_id: TypeId,
) -> (Vec<Identifier>, TypeId) {
    let mut next_id = cur_id;
    let mut result = Vec::new();
    for identifier in identifiers {
        let (ident_id, nxt_id) = next_id.next();
        let ident = tast::Identifier {
            tpe: ident_id,
            value: identifier.value,
        };
        next_id = nxt_id;
        result.push(ident);
    }
    (result, next_id)
}

fn unify(mut eqs: Equations) -> Result<Solutions, String> {
    use Equation::*;

    if eqs.is_empty() {
        return Err("Nothing to unify".into());
    }

    let mut solutions: HashMap<TypeId, Type> = HashMap::new();

    let mut did_something = false;
    let mut idx = 0;
    loop {
        match eqs.remove(idx) {
            IsEqual(Known(lhs_tpe), Known(rhs_tpe)) => {
                if lhs_tpe != rhs_tpe {
                    return Err(format!("Cannot unify {:?} and {:?}", lhs_tpe, rhs_tpe));
                }
                did_something = true;
            }
            IsEqual(Unknown(tid), Known(tpe)) => {
                if let Some(existing_sol) = solutions.get(&tid) {
                    if *existing_sol != tpe {
                        return Err(format!("Cannot unify {:?} and {:?}", existing_sol, tpe));
                    }
                }
                solutions.insert(tid, tpe);
                did_something = true;
            }
            IsEqual(Known(tpe), Unknown(tid)) => {
                if let Some(existing_sol) = solutions.get(&tid) {
                    if *existing_sol != tpe {
                        return Err(format!("Cannot unify {:?} and {:?}", tpe, existing_sol));
                    }
                }
                solutions.insert(tid, tpe);
                did_something = true;
            }
            IsEqual(Unknown(lhs), Unknown(rhs)) => {
                match (solutions.get(&lhs), solutions.get(&rhs)) {
                    (Some(lhs_solution), None) => {
                        let rhs_solution = lhs_solution.clone();
                        solutions.insert(rhs, rhs_solution);
                        did_something = true;
                    }
                    (None, Some(rhs_solution)) => {
                        let lhs_solution = rhs_solution.clone();
                        solutions.insert(lhs, lhs_solution);
                        did_something = true;
                    }
                    (Some(lhs_solution), Some(rhs_solution)) => {
                        if lhs_solution != rhs_solution {
                            return Err(format!(
                                "Cannot unify {:?} and {:?}",
                                lhs_solution, rhs_solution
                            ));
                        }
                        did_something = true;
                    }
                    (None, None) => {
                        // hopefully we have more info in the next iteration
                        // and can solve it then
                        eqs.insert(idx, IsEqual(Unknown(lhs), Unknown(rhs)))
                    }
                }
            }
        }

        if eqs.is_empty() {
            return Ok(solutions);
        }

        idx += 1;
        if idx >= eqs.len() {
            if !did_something {
                return Err("Cannot unify remaining equations".into());
            } else {
                did_something = false;
            }
            idx = 0;
        }
    }
}

fn set_types_program(program: &mut Program, eqs: &Solutions) {
    for mut stmt in program.0.iter_mut() {
        set_types_statement(&mut stmt, &eqs)
    }
}

fn set_types_statement(stmt: &mut Statement, eqs: &Solutions) {
    match stmt {
        Statement::Expression { value, .. } => set_types_expression(value, &eqs),
        Statement::Let { name, expression } => {
            set_tpe(&mut name.tpe, eqs);
            set_types_expression(expression, eqs);
        }
        Statement::Block { statements, .. } => {
            for stmt in statements {
                set_types_statement(stmt, &eqs)
            }
        }
        Statement::Return { value, .. } => set_types_expression(value, &eqs),
    }
}

fn set_types_expression(exp: &mut Expression, eqs: &Solutions) {
    use Expression::*;
    match exp {
        If {
            tpe,
            condition,
            consequence,
            alternative,
        } => {
            set_tpe(tpe, eqs);
            set_types_expression(condition, eqs);
            set_types_statement(consequence, eqs);
            if let Some(alt) = alternative {
                set_types_statement(alt, eqs);
            }
        }
        Infix { tpe, lhs, rhs, .. } => {
            set_tpe(tpe, eqs);
            set_types_expression(lhs, eqs);
            set_types_expression(rhs, eqs);
        }
        FunctionLiteral {
            return_type, body, ..
        } => {
            set_tpe(return_type, eqs);
            set_types_statement(body, eqs);
        }
        ArrayLiteral { tpe, values, .. } => {
            set_tpe(tpe, eqs);
            for value in values {
                set_types_expression(value, eqs);
            }
        }
        Index {
            tpe,
            container,
            index,
        } => {
            set_tpe(tpe, eqs);
            set_types_expression(container, eqs);
            set_types_expression(index, eqs);
        }
        Call {
            tpe,
            arguments,
            function,
        } => {
            set_tpe(tpe, eqs);
            for arg in arguments {
                set_types_expression(arg, eqs);
            }
            match function {
                Function::Identifier(ident) => set_tpe(&mut ident.tpe, eqs),
                Function::Literal {
                    return_type,
                    parameters,
                    body,
                } => {
                    set_tpe(return_type, eqs);
                    for param in parameters {
                        set_tpe(&mut param.tpe, eqs);
                    }
                    set_types_statement(body, eqs);
                }
            }
        }
        Identifier(ident) => set_tpe(&mut ident.tpe, eqs),
        IntLiteral { .. } => {}
        BooleanLiteral { .. } => {}
        StringLiteral { .. } => {}
        other => panic!("Unhandled expression in set_types {:?}", other),
    }
}

fn set_tpe(tpe: &mut TypeVariable, eqs: &Solutions) {
    match tpe {
        Unknown(tid) => {
            *tpe = Known(find_tpe_in_equations(&tid, eqs).expect("TypeId missing in Equations"))
        }
        Known(..) => {}
    }
}

fn find_tpe_in_equations(tid: &TypeId, eqs: &Solutions) -> Option<Type> {
    eqs.get(tid).cloned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_literal_inference() {
        assert_eq!(
            Known(Type::Int),
            infer_expression("let x = 5 + 5; x", 1).tpe()
        );
        assert_eq!(Known(Type::Int), infer_expression("let x = 5; x", 1).tpe());
        assert_eq!(
            Known(Type::Boolean),
            infer_expression("let x = true; x", 1).tpe()
        );
        assert_eq!(
            Known(Type::Boolean),
            infer_expression("let x = 5 == 5; x", 1).tpe()
        );
        assert_eq!(
            Known(Type::String_),
            infer_expression("let x = \"foo\"; x", 1).tpe()
        );
        assert_eq!(
            Known(Type::String_),
            infer_expression("let x = \"foo\"; x + \"bar\"", 1).tpe()
        );
    }

    #[test]
    fn test_identifier_inference() {
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("let x = 5; x")).tpe()
        );
        assert_eq!(
            Known(Type::Boolean),
            last(infer_statement(
                "let x = if (5 == 4) { 1==2 } else { false == true }; x"
            ))
            .tpe()
        );
        assert_eq!(
            Known(Type::Boolean),
            last(infer_statement(
                "let x = if (5 == 4) { let y = \"foo\"; y == \"bar\" } else { false == true }; x"
            ))
            .tpe()
        );
    }

    #[test]
    fn test_function_inference() {
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("let double = fn(x) { x + x }; double(1)")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(x) { x + 1 }")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(x) { x + x }(1)")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("let neg = fn(x) { x - (2 * x) }; neg(1)")).tpe()
        );
        assert_eq!(
            Known(Type::String_),
            last(infer_statement("fn(x, y) { x + y }(\"foo\", \"bar\");")).tpe()
        );
    }

    #[test]
    fn test_return_inference() {
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(x) { return x + 1 }")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("let double = fn() { return 1; }; double()")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(x) { return x + x }(1)")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(x) { let y = x + x; return y }(1)")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement(
                "let neg = fn(x) { return x - (2 * x) }; neg(1)"
            ))
            .tpe()
        );
    }

    #[test]
    fn test_if_inference() {
        match infer_expression("let x = 3; let y = 1; if (x == 5) { 5 + 5 } else { y }", 2) {
            Expression::If {
                tpe,
                condition,
                consequence,
                alternative,
            } => {
                assert_eq!(Known(Type::Int), tpe);
                assert_eq!(Known(Type::Boolean), condition.tpe());
                assert_eq!(Known(Type::Int), consequence.tpe());
                assert_eq!(Known(Type::Int), alternative.unwrap().tpe());
            }
            other => panic!("Expected int expression, got: {:?}", other),
        }
    }

    #[test]
    fn test_array_inference() {
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("let xs = [1, 2, 3]; xs[0]")).tpe()
        );
        assert_eq!(
            Known(Type::String_),
            last(infer_statement("let xs = [\"foo\"]; xs[0]")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(xs) { xs[1] }([1, 2])")).tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement(
                "let index = fn(xs, i) { xs[i] }; index([1], 0)"
            ))
            .tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement(
                "let nested = fn(xs, ys, i) { xs[ys[i]] }; nested([0], [0], 0)"
            ))
            .tpe()
        );
        assert_eq!(
            Known(Type::Int),
            last(infer_statement("fn(xs, i) { xs[i + i] }([1, 2, 3], 1)")).tpe()
        );
        assert_eq!(
            Known(Type::String_),
            last(infer_statement(
                "let s = \"x\"; let index = fn(xs, i) { xs[i] }; let xs = []; index(xs, 0) + s"
            ))
            .tpe()
        );
        assert_eq!(
            "Cannot unify String_ and Int".to_string(),
            infer_error("let xs = [1]; xs[\"x\"]")
        );
        assert_eq!(
            "Cannot unify Int and String_".to_string(),
            infer_error("fn(xs, s) { xs[s] }([1], \"x\")")
        );
        assert_eq!(
            "Cannot unify Int and Boolean".to_string(),
            infer_error("let xs = [1, 1 == 1]; ")
        );
    }

    #[test]
    fn test_errors() {
        assert_eq!(
            "Cannot unify Boolean and Int".to_string(),
            infer_error("let x = false; let y = 5; x + y")
        );
        assert_eq!(
            "Cannot unify Boolean and Int".to_string(),
            infer_error("if (5 == 4) { 1 } else { false }")
        );
        assert_eq!(
            "Cannot unify Int and String_".to_string(),
            infer_error("let x = 5 + \"x\"; x")
        );
        assert_eq!(
            "Function neg takes 2 parameters, but 1 given".to_string(),
            infer_error("let neg = fn(x) { x - (2 * x) }; neg(1, 2)")
        );
        assert_eq!(
            "Cannot unify Int and String_".to_string(),
            infer_error("let neg = fn(x) { x - (2 * x) }; neg(\"foo\")")
        );
        assert_eq!(
            "Unreachable statement(s) after return".to_string(),
            infer_error("fn() { return 1; 2 }()")
        );
    }

    fn infer_expression(input: &str, idx: usize) -> Expression {
        let mut statements = infer_statement(input);
        match statements.remove(idx) {
            Statement::Expression { value, .. } => value,
            other => panic!("Expected expression, but got: {:?}", other),
        }
    }

    fn infer_statement(input: &str) -> Vec<Statement> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        infer_types(*program).unwrap().0
    }

    fn last<X>(mut xs: Vec<X>) -> X {
        xs.remove(xs.len() - 1)
    }

    fn infer_error(input: &str) -> String {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        infer_types(*program).err().unwrap()
    }
}
