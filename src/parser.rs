use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{self, Token, TokenType};

use std::collections::HashMap;
use std::fmt::Debug;
use std::mem;

pub struct Parser {
    l: Lexer,

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: PrefixParseFn,
    infix_parse_fns: InfixParseFn,
}

type InfixParseFn = HashMap<TokenType, fn(&mut Parser, Expression) -> ParserResult<Expression>>;
type PrefixParseFn = HashMap<TokenType, fn(&mut Parser) -> ParserResult<Expression>>;

type ParserError = String;

type ParserResult<T> = Result<T, ParserError>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        let cur_token = l.next_token();
        let peek_token = l.next_token();

        let mut p = Parser {
            l,
            cur_token,
            peek_token,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(token::IDENT.into(), Parser::parse_identifier);
        p.register_prefix(token::INT.into(), Parser::parse_integer_literal);
        p.register_prefix(token::BANG.into(), Parser::parse_prefix_expression);
        p.register_prefix(token::MINUS.into(), Parser::parse_prefix_expression);
        p.register_prefix(token::TRUE.into(), Parser::parse_boolean_literal);
        p.register_prefix(token::FALSE.into(), Parser::parse_boolean_literal);
        p.register_prefix(token::LPAREN.into(), Parser::parse_grouped_expression);
        p.register_prefix(token::LBRACKET.into(), Parser::parse_array_expression);
        p.register_prefix(token::LBRACE.into(), Parser::parse_map_expression);
        p.register_prefix(token::IF.into(), Parser::parse_if_expression);
        p.register_prefix(token::FUNCTION.into(), Parser::parse_function_literal);
        p.register_prefix(token::STRING.into(), Parser::parse_string_literal);

        p.register_infix(token::PLUS.into(), Parser::parse_infix_expression);
        p.register_infix(token::MINUS.into(), Parser::parse_infix_expression);
        p.register_infix(token::SLASH.into(), Parser::parse_infix_expression);
        p.register_infix(token::ASTERISK.into(), Parser::parse_infix_expression);
        p.register_infix(token::EQ.into(), Parser::parse_infix_expression);
        p.register_infix(token::NEQ.into(), Parser::parse_infix_expression);
        p.register_infix(token::LT.into(), Parser::parse_infix_expression);
        p.register_infix(token::GT.into(), Parser::parse_infix_expression);
        p.register_infix(token::LPAREN.into(), Parser::parse_call_expression);
        p.register_infix(token::LBRACKET.into(), Parser::parse_index_expression);

        p
    }

    pub fn parse_program(&mut self) -> ParserResult<Box<Program>> {
        let mut statements: Vec<Statement> = vec![];

        while self.cur_token.tpe.as_str() != token::EOF {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(msg) => return Err(format!("Failed to parse: {}", msg)),
            }
            self.next_token();
        }

        Ok(Box::new(Program(statements)))
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.l.next_token());
    }

    fn cur_precedence(&self) -> Precedence {
        self.precedence(&self.cur_token.tpe)
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedence(&self.peek_token.tpe)
    }

    fn precedence(&self, tpe: &str) -> Precedence {
        match tpe {
            token::EQ => Precedence::Equals,
            token::NEQ => Precedence::Equals,
            token::LT => Precedence::Lessgreater,
            token::GT => Precedence::Lessgreater,
            token::PLUS => Precedence::Sum,
            token::MINUS => Precedence::Sum,
            token::SLASH => Precedence::Product,
            token::ASTERISK => Precedence::Product,
            token::LPAREN => Precedence::Call,
            token::LBRACKET => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    fn register_prefix(
        &mut self,
        tpe: TokenType,
        func: fn(&mut Parser) -> ParserResult<Expression>,
    ) {
        self.prefix_parse_fns.insert(tpe, func);
    }

    fn register_infix(
        &mut self,
        tpe: TokenType,
        func: fn(&mut Parser, Expression) -> ParserResult<Expression>,
    ) {
        self.infix_parse_fns.insert(tpe, func);
    }

    fn parse_identifier(&mut self) -> ParserResult<Expression> {
        Ok(Expression::Identifier(self.parse_raw_identifier()))
    }

    fn parse_raw_identifier(&mut self) -> Identifier {
        Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }
    }

    fn parse_grouped_expression(&mut self) -> ParserResult<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(token::RPAREN)?;

        Ok(exp)
    }

    fn parse_array_expression(&mut self) -> ParserResult<Expression> {
        let token = self.cur_token.clone();

        self.next_token();

        let mut values = Vec::new();
        while self.cur_token.tpe != token::RBRACKET {
            let value = self.parse_expression(Precedence::Lowest)?;
            values.push(value);

            self.next_token();
            if self.cur_token.tpe == token::COMMA {
                self.next_token();
            }
        }

        Ok(Expression::ArrayLiteral { token, values })
    }

    fn parse_map_expression(&mut self) -> ParserResult<Expression> {
        let token = self.cur_token.clone();

        self.next_token();

        let mut values = Vec::new();
        while self.cur_token.tpe != token::RBRACE {
            let key = self.parse_expression(Precedence::Lowest)?;
            self.next_token();

            self.skip_tpe(token::COLON)?;

            let value = self.parse_expression(Precedence::Lowest)?;

            values.push((key, value));

            self.next_token();
            if self.cur_token.tpe == token::COMMA {
                self.next_token();
            }
        }

        Ok(Expression::MapLiteral { token, values })
    }

    fn parse_function_literal(&mut self) -> ParserResult<Expression> {
        let token = self.cur_token.clone();

        self.expect_peek(token::LPAREN)?;

        self.next_token();

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(token::LBRACE)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> ParserResult<Vec<Identifier>> {
        let mut parameters = Vec::new();
        while self.cur_token.tpe != token::RPAREN {
            let parameter = self.parse_raw_identifier();
            parameters.push(parameter);

            self.next_token();
            if self.cur_token.tpe == token::COMMA {
                self.next_token();
            }
        }
        Ok(parameters)
    }

    fn parse_if_expression(&mut self) -> ParserResult<Expression> {
        let token = self.cur_token.clone();

        self.expect_peek(token::LPAREN)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(token::RPAREN)?;

        self.expect_peek(token::LBRACE)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token.tpe == token::ELSE {
            self.next_token();
            self.expect_peek(token::LBRACE)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(Box::new),
        })
    }

    fn parse_integer_literal(&mut self) -> ParserResult<Expression> {
        let value =
            self.cur_token.literal.parse().map_err(|_| {
                format!("expected integer literal, got '{}'", self.cur_token.literal)
            })?;
        let exp = Expression::IntLiteral {
            token: self.cur_token.clone(),
            value,
        };
        Ok(exp)
    }

    fn parse_boolean_literal(&mut self) -> ParserResult<Expression> {
        let value = match self.cur_token.literal.as_ref() {
            "true" => true,
            "false" => false,
            other => return Err(format!("Unexpected bool: '{}'", other)),
        };
        Ok(Expression::BooleanLiteral {
            token: self.cur_token.clone(),
            value,
        })
    }

    fn parse_string_literal(&mut self) -> ParserResult<Expression> {
        Ok(Expression::StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }

    fn parse_prefix_expression(&mut self) -> ParserResult<Expression> {
        let token = self.cur_token.clone();
        let op = self.cur_token.literal.clone();

        self.next_token();

        let rhs = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix {
            token,
            op,
            rhs: Box::new(rhs),
        })
    }

    fn parse_infix_expression(&mut self, lhs: Expression) -> ParserResult<Expression> {
        let token = self.cur_token.clone();
        let op = self.cur_token.literal.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let rhs = self.parse_expression(precedence)?;

        Ok(Expression::Infix {
            token,
            op,
            rhs: Box::new(rhs),
            lhs: Box::new(lhs),
        })
    }

    fn parse_call_expression(&mut self, expression: Expression) -> ParserResult<Expression> {
        let token = self.cur_token.clone();
        let function = Function::from_expression(expression);
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::Call {
            token,
            function,
            arguments,
        })
    }

    fn parse_index_expression(&mut self, container: Expression) -> ParserResult<Expression> {
        let token = self.cur_token.clone();

        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(token::RBRACKET)?;

        Ok(Expression::Index {
            token,
            container: Box::new(container),
            index: Box::new(index),
        })
    }

    fn parse_call_arguments(&mut self) -> ParserResult<Vec<Expression>> {
        if self.peek_token.tpe == token::RPAREN {
            self.next_token();
            return Ok(Vec::new());
        }

        self.next_token();

        let mut arguments = Vec::new();
        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token.tpe == token::COMMA {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(token::RPAREN)?;

        Ok(arguments)
    }

    fn parse_statement(&mut self) -> ParserResult<Statement> {
        match self.cur_token.tpe.as_str() {
            token::LET => self.parse_let_statement(),
            token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_statement(&mut self) -> ParserResult<Statement> {
        let token = self.cur_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while self.cur_token.tpe != token::RBRACE && self.cur_token.tpe != token::EOF {
            statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(Statement::Block { token, statements })
    }

    fn parse_let_statement(&mut self) -> ParserResult<Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        self.expect_tpe(token::IDENT)?;
        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        self.next_token();
        self.skip_tpe(token::ASSIGN)?;

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.tpe == token::SEMICOLON {
            self.next_token();
        }

        Ok(Statement::Let {
            token,
            name,
            expression: Box::new(expression),
        })
    }

    fn parse_return_statement(&mut self) -> ParserResult<Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.tpe == token::SEMICOLON {
            self.next_token();
        }

        Ok(Statement::Return {
            token,
            value: expression,
        })
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Statement> {
        let token = self.cur_token.clone();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.tpe == token::SEMICOLON {
            self.next_token();
        }

        Ok(Statement::Expression {
            token,
            value: expression,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParserResult<Expression> {
        let prefix = match self.prefix_parse_fns.get(&self.cur_token.tpe) {
            Some(func) => func,
            None => {
                return Err(format!(
                    "No prefix parsing function found for type {}",
                    self.cur_token.tpe
                ))
            }
        };

        let mut lhs = prefix(self)?;

        while self.peek_token.tpe != token::SEMICOLON && precedence < self.peek_precedence() {
            let infix = match self.infix_parse_fns.get(&self.peek_token.tpe).cloned() {
                Some(func) => func,
                None => {
                    return Err(format!(
                        "No infix parsing function found for type {}",
                        self.peek_token.tpe
                    ))
                }
            };

            self.next_token();
            lhs = infix(self, lhs)?;
        }

        Ok(lhs)
    }

    fn expect_peek(&mut self, tpe: &str) -> ParserResult<()> {
        if self.peek_token.tpe == tpe {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "Expected type {} but got {}",
                tpe, self.peek_token.tpe
            ))
        }
    }

    fn skip_tpe(&mut self, tpe: &str) -> ParserResult<()> {
        self.expect_tpe(tpe)?;
        self.next_token();
        Ok(())
    }

    fn expect_tpe(&mut self, tpe: &str) -> ParserResult<()> {
        if self.cur_token.tpe == tpe {
            Ok(())
        } else {
            Err(format!("expected {}, got {:?}", tpe, self.cur_token))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let input = "
          let x = 5;
          let y = 10;
          let foobar = 838383;
        ";

        let mut program = parse_statements(input);

        assert_eq!(3, program.len());

        let line1 = program.remove(0);
        let rhs = verify_let_statement(line1, "x");
        verify_int_literal(5, "5", &rhs);

        let line2 = program.remove(0);
        let rhs = verify_let_statement(line2, "y");
        verify_int_literal(10, "10", &rhs);

        let line3 = program.remove(0);
        let rhs = verify_let_statement(line3, "foobar");
        verify_int_literal(838383, "838383", &rhs);
    }

    #[test]
    fn test_print_let_statements() {
        let input = "let x = add(4, 5);";
        assert_eq!(
            "let x = add(4, 5)",
            format!(
                "{}",
                Parser::new(Lexer::new(input)).parse_program().unwrap()
            )
        );
    }

    #[test]
    fn test_parse_return_statement() {
        let input = "
          return 5;
          return 10;
          return 5 + 5;
          return add(15);
        ";

        let mut program = parse_statements(input);

        assert_eq!(4, program.len());
        let rhs = verify_return_statement(program.remove(0));
        verify_int_literal(5, "5", &rhs);

        let rhs = verify_return_statement(program.remove(0));
        verify_int_literal(10, "10", &rhs);

        let rhs = verify_return_statement(program.remove(0));
        match rhs {
            Expression::Infix { .. } => {}
            _ => panic!("not an infix expression"),
        }

        let rhs = verify_return_statement(program.remove(0));
        match rhs {
            Expression::Call { .. } => {}
            _ => panic!("not a call expression"),
        }
    }

    #[test]
    fn test_parse_expression_statement() {
        let input = "x + 10;";

        let program = parse_statements(input);

        assert_eq!(1, program.len());
        verify_expression_statement(&program[0]);
    }

    #[test]
    fn test_identifier_expression() {
        let exp = statement_expression("foobar;");

        match exp {
            Expression::Identifier(ident) => {
                assert_eq!("foobar", ident.value);
                assert_eq!("foobar", ident.token_literal());
            }
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_integer_expression() {
        let exp = statement_expression("5;");

        verify_int_literal(5, "5", &exp);
    }

    #[test]
    fn test_boolean_expression() {
        let exp = statement_expression("true;");
        verify_boolean_literal(true, &exp);

        let exp = statement_expression("false;");
        verify_boolean_literal(false, &exp);
    }

    #[test]
    fn test_string_expression() {
        let exp = statement_expression("\"hello, world\";");

        verify_string_literal("hello, world", &exp);
    }

    #[test]
    fn test_array_literal() {
        let exp = statement_expression("[1, \"fo, o\", fn(x) { x }]");

        let mut values = match exp {
            Expression::ArrayLiteral { values, .. } => values,
            other => panic!("Not an array: {:?}", other),
        };

        assert_eq!(3, values.len());
        verify_int_literal(1, "1", &values.remove(0));
        verify_string_literal("fo, o", &values.remove(0));
        match values.remove(0) {
            Expression::FunctionLiteral { .. } => {}
            _ => panic!("Expected function literal"),
        }
    }

    #[test]
    fn test_map_literal() {
        let exp = statement_expression("{\"key\": \"value\", 1: true}");

        let mut values = match exp {
            Expression::MapLiteral { values, .. } => values,
            other => panic!("Not a map: {:?}", other),
        };

        assert_eq!(2, values.len());
        let (key, value) = values.remove(0);
        verify_string_literal("key", &key);
        verify_string_literal("value", &value);

        let (key, value) = values.remove(0);
        verify_int_literal(1, "1", &key);
        verify_boolean_literal(true, &value);
    }

    #[test]
    fn test_map_expression_literal() {
        let exp = statement_expression("{\"key\": 0+1}");

        let mut values = match exp {
            Expression::MapLiteral { values, .. } => values,
            other => panic!("Not a map: {:?}", other),
        };

        assert_eq!(1, values.len());
        match values.remove(0).1 {
            Expression::Infix { .. } => {}
            other => panic!("not an infix: {:?}", other),
        }
    }

    #[test]
    fn test_empty_map_literal() {
        match statement_expression("{}") {
            Expression::MapLiteral { values, .. } => assert!(values.is_empty()),
            other => panic!("Not a map: {:?}", other),
        }
    }

    #[test]
    fn test_array_index_expression() {
        let exp = statement_expression("myArray[1 + 1]");

        let (array, index) = match exp {
            Expression::Index {
                container, index, ..
            } => (*container, *index),
            other => panic!("Not an array: {:?}", other),
        };

        match array {
            Expression::Identifier(..) => {}
            other => panic!("Expected array literal, but got: {:?}", other),
        }

        match index {
            Expression::Infix { .. } => {}
            other => panic!("Expected infix, but got: {:?}", other),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let exp = statement_expression("!5;");

        match exp {
            Expression::Prefix { token: _, op, rhs } => {
                assert_eq!("!", op);
                verify_int_literal(5, "5", &rhs);
            }
            _ => panic!("Expected prefix"),
        }

        let exp = statement_expression("-15;");

        match exp {
            Expression::Prefix { token: _, op, rhs } => {
                assert_eq!("-", op);
                verify_int_literal(15, "15", &rhs);
            }
            _ => panic!("Expected prefix"),
        }

        let exp = statement_expression("!true");

        match exp {
            Expression::Prefix { token: _, op, rhs } => {
                assert_eq!("!", op);
                verify_boolean_literal(true, &rhs);
            }
            _ => panic!("Expected prefix"),
        }
    }

    #[test]
    fn test_infix_expression() {
        verify_infix_int("5 + 5", 5, "+", 5);
        verify_infix_int("5 - 5", 5, "-", 5);
        verify_infix_int("5 * 5", 5, "*", 5);
        verify_infix_int("5 / 5", 5, "/", 5);
        verify_infix_int("5 > 5", 5, ">", 5);
        verify_infix_int("5 < 5", 5, "<", 5);
        verify_infix_int("5 == 5", 5, "==", 5);
        verify_infix_int("5 != 5", 5, "!=", 5);

        verify_infix_bool("true == true", true, "==", true);
        verify_infix_bool("true != false", true, "!=", false);
        verify_infix_bool("false == false", false, "==", false);
    }

    #[test]
    fn test_operator_precedence() {
        verify_precedence("-a * b", "((-a) * b)");
        verify_precedence("!-a", "(!(-a))");
        verify_precedence("a + b + c", "((a + b) + c)");
        verify_precedence("a + b - c", "((a + b) - c)");
        verify_precedence("a * b * c", "((a * b) * c)");
        verify_precedence("a * b / c", "((a * b) / c)");
        verify_precedence("a + b / c", "(a + (b / c))");
        verify_precedence("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
        verify_precedence("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)");
        verify_precedence("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
        verify_precedence("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
        verify_precedence(
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        );
        verify_precedence("true", "true");
        verify_precedence("false", "false");
        verify_precedence("3 > 5 == false", "((3 > 5) == false)");
        verify_precedence("3 < 5 == true", "((3 < 5) == true)");

        verify_precedence("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)");
        verify_precedence("(5 + 5) * 2", "((5 + 5) * 2)");
        verify_precedence("2 / (5 + 5)", "(2 / (5 + 5))");
        verify_precedence("-(5 + 5)", "(-(5 + 5))");
        verify_precedence("!(true == true)", "(!(true == true))");
        verify_precedence("a + add(b * c) + d", "((a + add((b * c))) + d)");
        verify_precedence(
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        );
        verify_precedence(
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        );
        verify_precedence(
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        );
        verify_precedence(
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        );
    }

    #[test]
    fn test_if_expression() {
        let exp = statement_expression("if (x < y) { x }");
        let (cond, stmts) = match exp {
            Expression::If {
                condition,
                consequence,
                alternative: None,
                ..
            } => {
                let statements = match *consequence {
                    Statement::Block { statements, .. } => statements,
                    _ => panic!("Expected block"),
                };
                (condition, statements)
            }
            _ => panic!("Expected if"),
        };

        verify_infix_ident(*cond, "x", "<", "y");
        assert_eq!(1, stmts.len());
        match &stmts[0] {
            Statement::Expression {
                value: Expression::Identifier(Identifier { value, .. }),
                ..
            } => assert_eq!("x", value),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_if_expression_alt() {
        let exp = statement_expression("if (x < y) { x } else { z }");
        let (cond, conds, alts) = match exp {
            Expression::If {
                condition,
                consequence,
                alternative: Some(alternative),
                ..
            } => {
                let conds = match *consequence {
                    Statement::Block {
                        statements: conds, ..
                    } => conds,
                    _ => panic!("Expected block"),
                };
                let alts = match *alternative {
                    Statement::Block {
                        statements: alts, ..
                    } => alts,
                    _ => panic!("Expected block"),
                };

                (condition, conds, alts)
            }
            _ => panic!("Expected if"),
        };

        verify_infix_ident(*cond, "x", "<", "y");
        assert_eq!(1, conds.len());
        match &conds[0] {
            Statement::Expression {
                value: Expression::Identifier(Identifier { value, .. }),
                ..
            } => assert_eq!("x", value),
            _ => panic!("Expected identifier"),
        }
        assert_eq!(1, alts.len());
        match &alts[0] {
            Statement::Expression {
                value: Expression::Identifier(Identifier { value, .. }),
                ..
            } => assert_eq!("z", value),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_function_literal() {
        let (parameters, body) = match statement_expression("fn(x, y) { x + y; }") {
            Expression::FunctionLiteral {
                parameters, body, ..
            } => (parameters, body),
            _ => panic!("not a function literal"),
        };

        assert_eq!(2, parameters.len());
        assert_eq!("x", parameters[0].value);
        assert_eq!("y", parameters[1].value);

        let body_stmt = match *body {
            Statement::Block { mut statements, .. } => {
                assert_eq!(1, statements.len());
                statements.remove(0)
            }
            unexpected => panic!("body is not a block, but {:?}", unexpected),
        };
        let body_exp = match body_stmt {
            Statement::Expression { value, .. } => value,
            _ => panic!("statement in body is not an expression"),
        };
        verify_infix_ident(body_exp, "x", "+", "y");
    }

    #[test]
    fn test_function_literal_one_param() {
        let (parameters, _) = match statement_expression("fn(x) { return 1; }") {
            Expression::FunctionLiteral {
                parameters, body, ..
            } => (parameters, body),
            _ => panic!("not a function literal"),
        };

        assert_eq!(1, parameters.len());
    }

    #[test]
    fn test_function_literal_no_params() {
        let (parameters, _) = match statement_expression("fn() { return 1; }") {
            Expression::FunctionLiteral {
                parameters, body, ..
            } => (parameters, body),
            _ => panic!("not a function literal"),
        };

        assert_eq!(0, parameters.len());
    }

    #[test]
    fn test_call_expression() {
        let (function, mut arguments) = match statement_expression("add(1, 2 * 3, 4 + 5);") {
            Expression::Call {
                function: Function::Identifier(function),
                arguments,
                ..
            } => (function, arguments),
            _ => panic!("not a call"),
        };

        assert_eq!("add", function.value);
        assert_eq!(3, arguments.len());
        verify_int_literal(1, "1", &arguments.remove(0));
    }

    #[test]
    fn test_string_function_parameter() {
        let mut arguments = match statement_expression("len(\"a\")") {
            Expression::Call { arguments, .. } => arguments,
            _ => panic!("not a call"),
        };

        assert_eq!(1, arguments.len());
        verify_string_literal("a", &arguments.remove(0));
    }

    fn verify_precedence(input: &str, expected: &str) {
        let mut p = Parser::new(Lexer::new(input));
        let program = p.parse_program().unwrap();
        assert_eq!(expected, format!("{}", program));
    }

    fn verify_infix_int(input: &str, exp_lhs: u32, exp_op: &str, exp_rhs: u32) {
        fn extract(exp: &Expression) -> u32 {
            match exp {
                Expression::IntLiteral { value, .. } => *value,
                _ => panic!("Expected bool"),
            }
        }
        let exp = statement_expression(input);
        verify_infix(&exp, extract, exp_lhs, exp_op, exp_rhs);
    }

    fn verify_infix_ident(exp: Expression, exp_lhs: &str, exp_op: &str, exp_rhs: &str) {
        fn extract(exp: &Expression) -> String {
            match exp {
                Expression::Identifier(Identifier { value, .. }) => value.to_string(),
                _ => panic!("Expected ident"),
            }
        }
        verify_infix(&exp, extract, exp_lhs.into(), exp_op, exp_rhs.into());
    }

    fn verify_infix_bool(input: &str, exp_lhs: bool, exp_op: &str, exp_rhs: bool) {
        fn extract(exp: &Expression) -> bool {
            match exp {
                Expression::BooleanLiteral { value, .. } => *value,
                _ => panic!("Expected bool"),
            }
        }
        let exp = statement_expression(input);
        verify_infix(&exp, &extract, exp_lhs, exp_op, exp_rhs);
    }

    fn verify_infix<T: Debug + PartialEq, F>(
        exp: &Expression,
        extract: F,
        exp_lhs: T,
        exp_op: &str,
        exp_rhs: T,
    ) where
        F: Fn(&Expression) -> T,
    {
        let (lhs, op, rhs) = match exp {
            Expression::Infix {
                token: _,
                op,
                lhs,
                rhs,
            } => (lhs, op, rhs),
            _ => panic!("Expected infix"),
        };

        assert_eq!(exp_op, op);
        assert_eq!(exp_lhs, extract(&lhs));
        assert_eq!(exp_rhs, extract(&rhs));
    }

    fn statement_expression(input: &str) -> Expression {
        let mut program = parse_statements(input);

        assert_eq!(1, program.len());
        match program.pop() {
            Some(Statement::Expression { value, .. }) => value,
            _ => panic!("Expected statement expression"),
        }
    }

    fn verify_int_literal(exp_value: u32, exp_literal: &str, exp: &Expression) {
        match exp {
            Expression::IntLiteral { token, value } => {
                assert_eq!(exp_value, *value);
                assert_eq!(exp_literal, token.literal);
            }
            _ => panic!("Expected int"),
        }
    }

    fn verify_boolean_literal(exp_value: bool, exp: &Expression) {
        match exp {
            Expression::BooleanLiteral { token, value } => {
                assert_eq!(exp_value, *value);
                assert_eq!(format!("{}", exp_value), token.literal);
            }
            _ => panic!("Expected bool"),
        }
    }

    fn verify_string_literal(exp_value: &str, exp: &Expression) {
        match exp {
            Expression::StringLiteral { value, .. } => {
                assert_eq!(exp_value, *value);
            }
            _ => panic!("Expected string"),
        }
    }

    fn verify_let_statement(s: Statement, name: &str) -> Expression {
        match s {
            Statement::Let {
                name: identifier,
                expression,
                ..
            } => {
                assert_eq!(name, &identifier.value);
                *expression
            }
            _ => panic!("Expected let statement"),
        }
    }

    fn verify_return_statement(s: Statement) -> Expression {
        match s {
            Statement::Return { value, .. } => value,
            _ => panic!("Expected return statement"),
        }
    }

    fn verify_expression_statement(s: &Statement) {
        match s {
            Statement::Expression { .. } => {}
            _ => panic!("Expected expression statement"),
        }
    }

    fn parse_statements(input: &str) -> Vec<Statement> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program().unwrap().0
    }
}
