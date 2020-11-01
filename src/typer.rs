use crate::ast;
use crate::tast::TypeVariable::*;
use crate::tast::{self, *};

pub fn infer_types(stmt: ast::Statement) -> Result<tast::Statement, String> {
    let assignments = assign_types_statement(stmt, TypeId::new()).0;
    println!("Assignments: {:?}", assignments);
    let mut equations = Vec::new();
    create_equations_statement(&assignments, &mut equations);
    println!("Equations: {:?}", equations);
    let solutions = unify(equations)?;
    println!("Solutions: {:?}", solutions);
    Ok(set_types(assignments, solutions))
}

fn assign_types_statement(stmt: ast::Statement, cur_id: TypeId) -> (tast::Statement, TypeId) {
    use ast::Statement::*;
    match stmt {
        Expression { token, value } => {
            let (exp, next_id) = assign_types_expression(value, cur_id);
            (tast::Statement::Expression { token, value: exp }, next_id)
        }
        Block { token, statements } => {
            let mut stmts = Vec::new();
            let mut next_id = cur_id;
            for stmt in statements {
                let (assigned, nxt_id) = assign_types_statement(stmt, next_id);
                stmts.push(assigned);
                next_id = nxt_id;
            }
            (
                tast::Statement::Block {
                    token,
                    statements: stmts,
                },
                next_id,
            )
        }
        other => panic!(
            "assign_types only handles Statement::Expression, given: {:?}",
            other
        ),
    }
}

fn assign_types_expression(exp: ast::Expression, cur_id: TypeId) -> (tast::Expression, TypeId) {
    use ast::Expression::*;
    match exp {
        IntLiteral { value, .. } => (tast::Expression::IntLiteral { value }, cur_id),
        StringLiteral { value, .. } => (tast::Expression::StringLiteral { value }, cur_id),
        Identifier(ident) => {
            // TODO: identifiers only get a new id when they are introduced (parameter, let binding).
            // otherwise they re-use it.
            // this means we'll have to pass down a map?
            let (id, next) = cur_id.next();
            (
                tast::Expression::Identifier {
                    ident: tast::Identifier { value: ident.value },
                    tpe: Unknown(id),
                },
                next,
            )
        }
        Prefix { op, rhs, .. } => {
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, cur_id);
            let (id, next) = next_id.next();
            let infix = tast::Expression::Prefix {
                tpe: Unknown(id),
                op: op,
                rhs: Box::new(rhs_assigned),
            };
            (infix, next)
        }
        Infix { op, rhs, lhs, .. } => {
            let (lhs_assigned, next_id) = assign_types_expression(*lhs, cur_id);
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, next_id);
            let (id, next) = next_id.next();
            let infix = tast::Expression::Infix {
                tpe: Unknown(id),
                op: op,
                lhs: Box::new(lhs_assigned),
                rhs: Box::new(rhs_assigned),
            };
            (infix, next)
        }
        If {
            condition: cond,
            consequence: then,
            alternative: mb_alt,
            ..
        } => {
            let (cond_assigned, next_id) = assign_types_expression(*cond, cur_id);
            let (then_assigned, next_id) = assign_types_statement(*then, next_id);
            let (mb_alt_assigned, next_id) = match mb_alt {
                Some(alt) => {
                    let (a_s, next) = assign_types_statement(*alt, next_id);
                    (Some(Box::new(a_s)), next)
                }
                None => (None, next_id),
            };
            let (id, next) = next_id.next();
            let if_exp = tast::Expression::If {
                tpe: Unknown(id),
                condition: Box::new(cond_assigned),
                consequence: Box::new(then_assigned),
                alternative: mb_alt_assigned,
            };
            (if_exp, next)
        }
        other => panic!("Unhandled expression: {:?}", other),
    }
}

fn create_equations_statement(stmt: &tast::Statement, equations: &mut tast::Equations) {
    use tast::Statement::*;
    match stmt {
        Expression { value, .. } => create_equations_expression(value, equations),
        Block { statements, .. } => {
            for stmt in statements {
                create_equations_statement(stmt, equations);
            }
        }
        other => panic!(
            "create_equations only handles Statement::Expression, given: {:?}",
            other
        ),
    }
}

fn create_equations_expression(exp: &tast::Expression, equations: &mut tast::Equations) {
    use tast::Expression::*;
    match exp {
        Infix {
            op, lhs, rhs, tpe, ..
        } => match op.as_str() {
            "+" | "-" => {
                equations.push(Equation::IsEqual(tpe.clone(), Known(Type::Int)));
                equations.push(Equation::IsEqual(lhs.tpe(), rhs.tpe()));
                create_equations_expression(lhs, equations);
                create_equations_expression(rhs, equations);
            }
            "==" => {
                equations.push(Equation::IsEqual(tpe.clone(), Known(Type::Boolean)));
                equations.push(Equation::IsEqual(lhs.tpe(), rhs.tpe()));
                create_equations_expression(lhs, equations);
                create_equations_expression(rhs, equations);
            }
            other => panic!("Unhandled operation: {:?}", other),
        },
        If {
            condition: cond,
            consequence: then,
            alternative: mb_alt,
            tpe,
            ..
        } => {
            equations.push(Equation::IsEqual(cond.tpe(), Known(Type::Boolean)));
            equations.push(Equation::IsEqual(tpe.clone(), then.tpe()));
            create_equations_expression(cond, equations);
            create_equations_statement(then, equations);

            if let Some(alt) = mb_alt {
                equations.push(Equation::IsEqual(tpe.clone(), alt.tpe()));
                equations.push(Equation::IsEqual(alt.tpe(), then.tpe()));
                create_equations_statement(alt, equations);
            }
        }
        Identifier { .. } => {}
        IntLiteral { .. } => {}
        other => panic!("Unhandled expression: {:?}", other),
    }
}

fn unify(mut eqs: Equations) -> Result<Equations, String> {
    use tast::Equation::*;

    let mut prev_known_rs = 0;
    loop {
        let known_rs: Vec<(TypeId, Type)> = eqs
            .clone()
            .into_iter()
            .filter_map(|IsEqual(lhs, rhs)| {
                if let (Unknown(l), Known(r)) = (lhs, rhs) {
                    Some((l, r))
                } else {
                    None
                }
            })
            .collect();

        if known_rs.len() <= prev_known_rs {
            return Ok(eqs);
        } else {
            prev_known_rs = known_rs.len();
        }

        for (tid, tpe) in known_rs {
            replace_rhs(&mut eqs, tid, tpe)
        }
    }
}

fn replace_rhs(eqs: &mut Equations, tid: TypeId, tpe: Type) {
    use tast::Equation::*;
    for IsEqual(lhs, rhs) in eqs.iter_mut() {
        if let Unknown(rhs_id) = rhs {
            if rhs_id == &tid {
                *rhs = Known(tpe.clone());
            }
        }
    }
}

fn set_types(_stmt: tast::Statement, _solutions: Equations) -> tast::Statement {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_literal_inference() {
        let exp = infer_expression("5");
        assert_is_int(exp)
    }

    #[test]
    fn test_infix_inference() {
        infer_expression("if (x == 5) { 5 + 5 } else { y }");
    }

    fn assert_is_int(exp: tast::Expression) {
        match exp {
            tast::Expression::IntLiteral { .. } => {}
            other => panic!("Expected int, but go: {:?}", other),
        }
    }

    fn infer_expression(input: &str) -> tast::Expression {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let statement = p.parse_program().unwrap().0.remove(0);
        match infer_types(statement) {
            Ok(tast::Statement::Expression { value, .. }) => value,
            other => panic!("Expected expression, but got: {:?}", other),
        }
    }
}
