// Tiny Smalltalk Parser
// Parses tokens into an AST

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    Integer(i32),
    Float(f64),
    String(String),
    Symbol(String),
    Character(char),
    True,
    False,
    Nil,

    // Variables
    Variable(String),
    Self_,

    // Assignment
    Assign(String, Box<Expr>),

    // Messages
    UnaryMessage(Box<Expr>, String),                    // receiver msg
    BinaryMessage(Box<Expr>, String, Box<Expr>),        // receiver op arg
    KeywordMessage(Box<Expr>, Vec<(String, Expr)>),     // receiver kw1: arg1 kw2: arg2

    // Blocks
    Block(Vec<String>, Vec<Expr>),  // [:arg1 :arg2 | body]

    // Return
    Return(Box<Expr>),

    // Sequence (statements separated by .)
    Sequence(Vec<Expr>),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        if self.current() == expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.current()))
        }
    }

    // Parse a complete program (sequence of statements)
    pub fn parse_program(&mut self) -> Result<Expr, String> {
        let mut statements = Vec::new();

        while *self.current() != Token::EOF {
            let stmt = self.parse_statement()?;
            statements.push(stmt);

            // Optional period separator
            if *self.current() == Token::Period {
                self.advance();
            }
        }

        if statements.len() == 1 {
            Ok(statements.pop().unwrap())
        } else {
            Ok(Expr::Sequence(statements))
        }
    }

    // Parse a single statement
    fn parse_statement(&mut self) -> Result<Expr, String> {
        // Return statement
        if *self.current() == Token::Caret {
            self.advance();
            let expr = self.parse_expression()?;
            return Ok(Expr::Return(Box::new(expr)));
        }

        // Check for assignment: identifier := expr
        if let Token::Identifier(name) = self.current().clone() {
            let saved_pos = self.pos;
            self.advance();
            if *self.current() == Token::Assign {
                self.advance();
                let expr = self.parse_expression()?;
                return Ok(Expr::Assign(name, Box::new(expr)));
            }
            // Not an assignment, backtrack
            self.pos = saved_pos;
        }

        self.parse_expression()
    }

    // Parse expression (keyword message level)
    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_keyword_message()
    }

    // Keyword message: receiver kw1: arg1 kw2: arg2
    fn parse_keyword_message(&mut self) -> Result<Expr, String> {
        let receiver = self.parse_binary_message()?;

        // Check for keyword message
        if let Token::Keyword(_) = self.current() {
            let mut keywords = Vec::new();
            while let Token::Keyword(kw) = self.current().clone() {
                self.advance();
                let arg = self.parse_binary_message()?;
                keywords.push((kw, arg));
            }
            return Ok(Expr::KeywordMessage(Box::new(receiver), keywords));
        }

        Ok(receiver)
    }

    // Binary message: receiver op arg op arg ...
    fn parse_binary_message(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary_message()?;

        while let Token::BinaryOp(op) = self.current().clone() {
            self.advance();
            let right = self.parse_unary_message()?;
            left = Expr::BinaryMessage(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    // Unary message: receiver msg msg msg ...
    fn parse_unary_message(&mut self) -> Result<Expr, String> {
        let mut receiver = self.parse_primary()?;

        while let Token::Identifier(msg) = self.current().clone() {
            // Make sure it's not followed by := (assignment)
            let saved_pos = self.pos;
            self.advance();
            if *self.current() == Token::Assign {
                self.pos = saved_pos;
                break;
            }
            // Check it's not a keyword (would have colon)
            if let Token::Keyword(_) = self.current() {
                self.pos = saved_pos;
                break;
            }
            receiver = Expr::UnaryMessage(Box::new(receiver), msg);
        }

        Ok(receiver)
    }

    // Primary: literal, variable, block, or parenthesized expression
    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.current().clone() {
            Token::Integer(n) => {
                self.advance();
                Ok(Expr::Integer(n))
            }
            Token::Float(f) => {
                self.advance();
                Ok(Expr::Float(f))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::Symbol(s) => {
                self.advance();
                Ok(Expr::Symbol(s))
            }
            Token::Character(c) => {
                self.advance();
                Ok(Expr::Character(c))
            }
            Token::True => {
                self.advance();
                Ok(Expr::True)
            }
            Token::False => {
                self.advance();
                Ok(Expr::False)
            }
            Token::Nil => {
                self.advance();
                Ok(Expr::Nil)
            }
            Token::Self_ => {
                self.advance();
                Ok(Expr::Self_)
            }
            Token::Identifier(name) => {
                self.advance();
                Ok(Expr::Variable(name))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Token::LBracket => {
                self.parse_block()
            }
            _ => Err(format!("Unexpected token: {:?}", self.current())),
        }
    }

    // Block: [:arg1 :arg2 | statements]
    fn parse_block(&mut self) -> Result<Expr, String> {
        self.expect(&Token::LBracket)?;

        let mut args = Vec::new();
        let mut body = Vec::new();

        // Parse block arguments [:x :y |
        while *self.current() == Token::Colon {
            self.advance();
            if let Token::Identifier(name) = self.current().clone() {
                self.advance();
                args.push(name);
            } else {
                return Err("Expected identifier after : in block argument".to_string());
            }
        }

        // If we had arguments, expect |
        if !args.is_empty() {
            self.expect(&Token::Pipe)?;
        }

        // Parse body statements
        while *self.current() != Token::RBracket {
            let stmt = self.parse_statement()?;
            body.push(stmt);

            if *self.current() == Token::Period {
                self.advance();
            }
        }

        self.expect(&Token::RBracket)?;

        Ok(Expr::Block(args, body))
    }
}

pub fn parse(tokens: &[Token]) -> Result<Expr, String> {
    let mut parser = Parser::new(tokens.to_vec());
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_integer() {
        let tokens = tokenize("42").unwrap();
        let ast = parse(&tokens).unwrap();
        match ast {
            Expr::Integer(42) => {}
            _ => panic!("Expected Integer(42)"),
        }
    }

    #[test]
    fn test_binary() {
        let tokens = tokenize("3 + 4").unwrap();
        let ast = parse(&tokens).unwrap();
        match ast {
            Expr::BinaryMessage(_, op, _) => assert_eq!(op, "+"),
            _ => panic!("Expected BinaryMessage"),
        }
    }

    #[test]
    fn test_unary_chain() {
        let tokens = tokenize("3 negated abs").unwrap();
        let ast = parse(&tokens).unwrap();
        match ast {
            Expr::UnaryMessage(_, msg) => assert_eq!(msg, "abs"),
            _ => panic!("Expected UnaryMessage"),
        }
    }

    #[test]
    fn test_block() {
        let tokens = tokenize("[:x | x + 1]").unwrap();
        let ast = parse(&tokens).unwrap();
        match ast {
            Expr::Block(args, _) => assert_eq!(args, vec!["x"]),
            _ => panic!("Expected Block"),
        }
    }
}
