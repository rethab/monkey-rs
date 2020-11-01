use crate::ast;
use crate::tast::TypeVariable::*;
use crate::tast::{self, *};

pub fn infer_types(stmt: ast::Statement) -> Result<Statement, String> {
    let mut assignments = assign_types_statement(stmt, TypeId::new()).0;
    println!("Assignments: {:?}", assignments);
    let mut equations = Vec::new();
    create_equations_statement(&assignments, &mut equations);
    println!("Equations: {:?}", equations);
    let solutions = unify(equations)?;
    println!("Solutions: {:?}", solutions);
    set_types_statement(&mut assignments, &solutions);
    Ok(assignments)
}

fn assign_types_statement(stmt: ast::Statement, cur_id: TypeId) -> (Statement, TypeId) {
    use ast::Statement::*;
    match stmt {
        Expression { token, value } => {
            let (exp, next_id) = assign_types_expression(value, cur_id);
            (Statement::Expression { token, value: exp }, next_id)
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
                Statement::Block {
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

fn assign_types_expression(exp: ast::Expression, cur_id: TypeId) -> (Expression, TypeId) {
    use ast::Expression::*;
    match exp {
        IntLiteral { value, .. } => (Expression::IntLiteral { value }, cur_id),
        StringLiteral { value, .. } => (Expression::StringLiteral { value }, cur_id),
        BooleanLiteral { value, .. } => (Expression::BooleanLiteral { value }, cur_id),
        Identifier(ident) => {
            // TODO: identifiers only get a new id when they are introduced (parameter, let binding).
            // otherwise they re-use it.
            // this means we'll have to pass down a map?
            let (id, next) = cur_id.next();
            (
                Expression::Identifier {
                    ident: tast::Identifier { value: ident.value },
                    tpe: Unknown(id),
                },
                next,
            )
        }
        Prefix { op, rhs, .. } => {
            let (rhs_assigned, next_id) = assign_types_expression(*rhs, cur_id);
            let (id, next) = next_id.next();
            let infix = Expression::Prefix {
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
            let infix = Expression::Infix {
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
            let if_exp = Expression::If {
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

fn create_equations_statement(stmt: &Statement, equations: &mut Equations) {
    use Statement::*;
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

fn create_equations_expression(exp: &Expression, equations: &mut Equations) {
    use Expression::*;
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
        BooleanLiteral { .. } => {}
        other => panic!("Unhandled expression: {:?}", other),
    }
}

fn unify(mut eqs: Equations) -> Result<Equations, String> {
    use Equation::*;

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
    use Equation::*;
    for IsEqual(_, rhs) in eqs.iter_mut() {
        if let Unknown(rhs_id) = rhs {
            if rhs_id == &tid {
                *rhs = Known(tpe.clone());
            }
        }
    }
}

fn set_types_statement(stmt: &mut Statement, eqs: &Equations) {
    match stmt {
        Statement::Expression { value, .. } => set_types_expression(value, &eqs),
        Statement::Block { statements, .. } => {
            for stmt in statements {
                set_types_statement(stmt, &eqs)
            }
        }
        other => panic!("Unhandled statement in set_types {:?}", other),
    }
}

fn set_types_expression(exp: &mut Expression, eqs: &Equations) {
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
        Identifier { tpe, .. } => set_unknown_tpe(tpe, eqs),
        IntLiteral { .. } => {}
        BooleanLiteral { .. } => {}
        other => panic!("Unhandled expression in set_types {:?}", other),
    }
}

fn set_unknown_tpe(tpe: &mut TypeVariable, eqs: &Equations) {
    match tpe {
        Unknown(tid) => {
            *tpe = Known(find_tpe_in_equations(&tid, eqs).expect("TypeId missing in Equations"))
        }
        Known(..) => {}
    }
}

fn find_tpe_in_equations(tid: &TypeId, eqs: &Equations) -> Option<Type> {
    for Equation::IsEqual(lhs, rhs) in eqs.iter() {
        if let (Unknown(l), Known(r)) = (lhs, rhs) {
            if l == tid {
                return Some(r.clone());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_literal_inference() {
        assert_eq!(Known(Type::Int), infer_expression("5").tpe());
        assert_eq!(Known(Type::Int), infer_expression("5 + 5").tpe());
        assert_eq!(Known(Type::Boolean), infer_expression("true").tpe());
        assert_eq!(Known(Type::Boolean), infer_expression("5 == 5").tpe());
    }

    #[test]
    fn test_if_inference() {
        match infer_expression("if (x == 5) { 5 + 5 } else { y }") {
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

    fn infer_expression(input: &str) -> Expression {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let statement = p.parse_program().unwrap().0.remove(0);
        match infer_types(statement) {
            Ok(Statement::Expression { value, .. }) => value,
            other => panic!("Expected expression, but got: {:?}", other),
        }
    }
}
