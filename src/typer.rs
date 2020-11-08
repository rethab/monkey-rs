use crate::ast;
use crate::tast::TypeVariable::*;
use crate::tast::{self, *};
use std::collections::HashMap;

type EquationsResult<T> = Result<T, String>;

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
            create_equations_expression(&exp, equations, ctx.clone())?;
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

            let next_ctx = if let tast::Expression::FunctionLiteral { parameters, .. } = exp {
                ctx.with_function(ident.clone(), parameters.clone())
            } else {
                ctx
            };

            equations.push(Equation::IsEqual(ident.tpe(), exp.tpe()));
            create_equations_expression(&exp, equations, ctx.clone())?;

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
        other => panic!(
            "assign_types only handles Statement::Expression, given: {:?}",
            other
        ),
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
    let mut sub_ctx = ctx.clone();
    for stmt in statements {
        let (assigned, nxt_id, next_ctx) =
            assign_types_statement(stmt, equations, next_id, sub_ctx)?;
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
        Prefix { op, rhs, .. } => {
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, equations, cur_id, ctx)?;
            let (id, next) = next_id.next();
            let infix = Expression::Prefix {
                tpe: id,
                op,
                rhs: Box::new(rhs_assigned),
            };
            Ok((infix, next))
        }
        Infix { op, rhs, lhs, .. } => {
            let (lhs_assigned, next_id) =
                assign_types_expression(*lhs, equations, cur_id, ctx.clone())?;
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, equations, next_id, ctx)?;
            let (tpe, next) = next_id.next();
            let infix = Expression::Infix {
                tpe,
                op,
                lhs: Box::new(lhs_assigned),
                rhs: Box::new(rhs_assigned),
            };
            match op.as_str() {
                "-" | "*" | "/" => {
                    equations.push(Equation::IsEqual(tpe.clone(), Known(Type::Int)));
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                    create_equations_expression(&lhs_assigned, equations, ctx)?;
                    create_equations_expression(&rhs_assigned, equations, ctx)?;
                }
                "+" => {
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                    equations.push(Equation::IsEqual(tpe.clone(), rhs_assigned.tpe()));
                    create_equations_expression(&lhs_assigned, equations, ctx)?;
                    create_equations_expression(&rhs_assigned, equations, ctx)?;
                }
                "==" => {
                    equations.push(Equation::IsEqual(tpe.clone(), Known(Type::Boolean)));
                    equations.push(Equation::IsEqual(lhs_assigned.tpe(), rhs_assigned.tpe()));
                    create_equations_expression(&lhs_assigned, equations, ctx)?;
                    create_equations_expression(&rhs_assigned, equations, ctx)?;
                }
            }
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

            if let Some(alt_assigned) = mb_alt_assigned {
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
            equations.push(Equation::IsEqual(function_id, body.tpe()));
            let function = Expression::FunctionLiteral {
                return_type: function_id,
                parameters,
                body: Box::new(body),
            };
            Ok((function, next_id))
        }
        Call {
            function,
            arguments,
            ..
        } => {
            let (func, mut next_id) = match function {
                ast::Function::Identifier(identifier) => {
                    let (mut identifier, next_id) =
                        assign_types_identifiers(vec![identifier].to_vec(), cur_id);
                    (Function::Identifier(identifier.remove(0)), next_id)
                }
                ast::Function::Literal {
                    parameters, body, ..
                } => {
                    let (parameters, body, next_id) = assign_types_function_literal(
                        parameters,
                        equations,
                        *body,
                        cur_id,
                        ctx.clone(),
                    )?;
                    let (function_id, next_id) = next_id.next();
                    let func = Function::Literal {
                        return_type: function_id,
                        parameters,
                        body: Box::new(body),
                    };
                    (func, next_id)
                }
            };

            let mut args = Vec::new();
            for argument in arguments {
                let (arg, nxt_id) =
                    assign_types_expression(argument, equations, next_id, ctx.clone())?;
                args.push(arg);
                next_id = nxt_id;
            }

            let (call_id, next_id) = next_id.next();
            let call = Expression::Call {
                tpe: call_id,
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
    let (mut stmts, next_id) =
        assign_types_statements(statements, equations, next_id, sub_ctx.clone())?;

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

fn create_equations_expression(
    exp: &Expression,
    equations: &mut Equations,
    ctx: Context,
) -> EquationsResult<()> {
    use Expression::*;
    match exp {
        Call {
            tpe,
            function,
            arguments,
        } => {
            this needs to be integrated into the type_assign block above
            equations.push(Equation::IsEqual(tpe.clone(), function.tpe()));
            match function {
                Function::Identifier(identifier) => {
                    equations.push(Equation::IsEqual(identifier.tpe(), tpe.clone()));
                    if let Some(parameters) = ctx.lookup_function(&identifier) {
                        if arguments.len() != parameters.len() {
                            return Err(format!(
                                "Function {} takes {} parameters, but {} given",
                                identifier.value,
                                arguments.len(),
                                parameters.len()
                            ));
                        }
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            equations.push(Equation::IsEqual(param.tpe(), arg.tpe()));
                        }
                        Ok(())
                    } else {
                        Err(format!("Function {} not found", identifier.value))
                    }
                }
                Function::Literal {
                    return_type,
                    parameters,
                    body,
                } => {
                    equations.push(Equation::IsEqual(return_type.clone(), tpe.clone()));
                    equations.push(Equation::IsEqual(return_type.clone(), body.tpe()));
                    equations.push(Equation::IsEqual(body.tpe(), tpe.clone()));
                    if arguments.len() != parameters.len() {
                        return Err(format!(
                            "Function <anonymous> takes {} parameters, but {} given",
                            arguments.len(),
                            parameters.len()
                        ));
                    }
                    for (param, arg) in parameters.iter().zip(arguments.iter()) {
                        equations.push(Equation::IsEqual(param.tpe(), arg.tpe()));
                    }
                    Ok(())
                }
            }
        }
        other => panic!(
            "create_equations_expression: Unhandled expression: {:?}",
            other
        ),
    }
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
            set_unknown_tpe(&mut name.tpe, eqs);
            set_types_expression(expression, eqs);
        }
        Statement::Block { statements, .. } => {
            for stmt in statements {
                set_types_statement(stmt, &eqs)
            }
        }
        other => panic!("Unhandled statement in set_types {:?}", other),
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
            set_unknown_tpe(tpe, eqs);
            set_types_expression(condition, eqs);
            set_types_statement(consequence, eqs);
            if let Some(alt) = alternative {
                set_types_statement(alt, eqs);
            }
        }
        Infix { tpe, lhs, rhs, .. } => {
            set_unknown_tpe(tpe, eqs);
            set_types_expression(lhs, eqs);
            set_types_expression(rhs, eqs);
        }
        FunctionLiteral {
            return_type, body, ..
        } => {
            set_unknown_tpe(return_type, eqs);
            set_types_statement(body, eqs);
        }
        Identifier(ident) => set_unknown_tpe(&mut ident.tpe, eqs),
        IntLiteral { .. } => {}
        BooleanLiteral { .. } => {}
        StringLiteral { .. } => {}
        other => panic!("Unhandled expression in set_types {:?}", other),
    }
}

fn set_unknown_tpe(tpe: &mut TypeVariable, eqs: &Solutions) {
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
            "Wrong number of arguments".to_string(),
            infer_error("let neg = fn(x) { x - (2 * x) }; neg(1, 2)")
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
