use crate::token;
use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.into(),
            position: 0,
            read_position: 0,
            ch: '0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        tpe: token::EQ.into(),
                        literal: "==".into(),
                    }
                } else {
                    new_token(token::ASSIGN.into(), self.ch)
                }
            }
            '+' => new_token(token::PLUS.into(), self.ch),
            '-' => new_token(token::MINUS.into(), self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        tpe: token::NEQ.into(),
                        literal: "!=".into(),
                    }
                } else {
                    new_token(token::BANG.into(), self.ch)
                }
            }
            '/' => new_token(token::SLASH.into(), self.ch),
            '*' => new_token(token::ASTERISK.into(), self.ch),
            '<' => new_token(token::LT.into(), self.ch),
            '>' => new_token(token::GT.into(), self.ch),
            ';' => new_token(token::SEMICOLON.into(), self.ch),
            ':' => new_token(token::COLON.into(), self.ch),
            '(' => new_token(token::LPAREN.into(), self.ch),
            ')' => new_token(token::RPAREN.into(), self.ch),
            ',' => new_token(token::COMMA.into(), self.ch),
            '{' => new_token(token::LBRACE.into(), self.ch),
            '}' => new_token(token::RBRACE.into(), self.ch),
            '[' => new_token(token::LBRACKET.into(), self.ch),
            ']' => new_token(token::RBRACKET.into(), self.ch),
            '\0' => Token {
                tpe: token::EOF.into(),
                literal: String::new(),
            },
            '"' => {
                self.read_char();
                let string = self.read_string();
                Token {
                    tpe: token::STRING.into(),
                    literal: string,
                }
            }

            _ => {
                if self.ch.is_alphabetic() {
                    let ident = self.read_identifier();
                    let tpe = lookup_keyword(&ident).unwrap_or_else(|| token::IDENT.into());
                    return Token {
                        tpe,
                        literal: ident,
                    };
                } else if self.ch.is_digit(10) {
                    return Token {
                        tpe: token::INT.into(),
                        literal: self.read_number(),
                    };
                } else {
                    new_token(token::ILLEGAL.into(), self.ch)
                }
            }
        };

        self.read_char();
        tok
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();
        while self.ch.is_digit(10) {
            number.push(self.ch);
            self.read_char();
        }
        number
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while self.ch.is_alphabetic() {
            ident.push(self.ch);
            self.read_char();
        }
        ident
    }

    fn read_string(&mut self) -> String {
        let mut string = String::new();
        while self.ch != '"' {
            string.push(self.ch);
            self.read_char();
        }
        string
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }
}

fn new_token(tpe: TokenType, ch: char) -> Token {
    Token {
        tpe,
        literal: ch.to_string(),
    }
}

fn lookup_keyword(ident: &str) -> Option<TokenType> {
    match ident {
        "fn" => Some(token::FUNCTION.into()),
        "let" => Some(token::LET.into()),
        "true" => Some(token::TRUE.into()),
        "false" => Some(token::FALSE.into()),
        "if" => Some(token::IF.into()),
        "else" => Some(token::ELSE.into()),
        "return" => Some(token::RETURN.into()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token;

    #[test]
    fn next_token() {
        let input = " 
          let five = 5;
          let ten = 10;
          let add = fn(x, y) { x + y; };
          let result = add(five, ten);
          !-/*5;
          5 < 10 > 5;

          if (5 < 10) { 
            return true;
          } else {
            return false;
          }

          10 == 10;
          10 != 9;
          \"foobar\"
          \"foo bar\";
          [1, \"foo\"];
          {\"key\": \"value\", true: 2};
        ";
        let tests = vec![
            (token::LET, "let"),
            (token::IDENT, "five"),
            (token::ASSIGN, "="),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "ten"),
            (token::ASSIGN, "="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "add"),
            (token::ASSIGN, "="),
            (token::FUNCTION, "fn"),
            (token::LPAREN, "("),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::IDENT, "y"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "result"),
            (token::ASSIGN, "="),
            (token::IDENT, "add"),
            (token::LPAREN, "("),
            (token::IDENT, "five"),
            (token::COMMA, ","),
            (token::IDENT, "ten"),
            (token::RPAREN, ")"),
            (token::SEMICOLON, ";"),
            (token::BANG, "!"),
            (token::MINUS, "-"),
            (token::SLASH, "/"),
            (token::ASTERISK, "*"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::GT, ">"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::IF, "if"),
            (token::LPAREN, "("),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::TRUE, "true"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::ELSE, "else"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::FALSE, "false"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::INT, "10"),
            (token::EQ, "=="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::INT, "10"),
            (token::NEQ, "!="),
            (token::INT, "9"),
            (token::SEMICOLON, ";"),
            (token::STRING, "foobar"),
            (token::STRING, "foo bar"),
            (token::SEMICOLON, ";"),
            (token::LBRACKET, "["),
            (token::INT, "1"),
            (token::COMMA, ","),
            (token::STRING, "foo"),
            (token::RBRACKET, "]"),
            (token::SEMICOLON, ";"),
            (token::LBRACE, "{"),
            (token::STRING, "key"),
            (token::COLON, ":"),
            (token::STRING, "value"),
            (token::COMMA, ","),
            (token::TRUE, "true"),
            (token::COLON, ":"),
            (token::INT, "2"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (i, tt) in tests {
            let tok = lexer.next_token();
            assert_eq!(i, tok.tpe, "parsed token: {:?}", tok);
            assert_eq!(tt, tok.literal, "parsed token: {:?}", tok);
        }
    }

    #[test]
    fn int_literal() {
        let tok = Lexer::new("5").next_token();

        assert_eq!(token::INT, tok.tpe);
        assert_eq!("5", tok.literal);
    }
}
