// Tiny Smalltalk Lexer
// Tokenizes Smalltalk source code

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Integer(i32),
    Float(f64),          // Floating point literal
    String(String),
    Symbol(String),      // #symbol
    Character(char),     // $c

    // Identifiers
    Identifier(String),  // variable or unary message
    Keyword(String),     // keyword: (includes colon)
    BinaryOp(String),    // + - * / < > = etc.

    // Delimiters
    LParen,              // (
    RParen,              // )
    LBracket,            // [
    RBracket,            // ]
    Period,              // .
    Caret,               // ^ (return)
    Assign,              // :=
    Pipe,                // |
    Colon,               // :

    // Special
    True,
    False,
    Nil,
    Self_,

    EOF,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = source.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];

        // Skip whitespace
        if c.is_whitespace() {
            i += 1;
            continue;
        }

        // Skip comments "comment"
        if c == '"' {
            i += 1;
            while i < chars.len() && chars[i] != '"' {
                i += 1;
            }
            i += 1; // skip closing "
            continue;
        }

        // Number (integer or float)
        if c.is_ascii_digit() || (c == '-' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit()) {
            let start = i;
            if c == '-' {
                i += 1;
            }
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
            }
            // Check for decimal point followed by digits (float)
            if i < chars.len() && chars[i] == '.' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                i += 1; // skip decimal point
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                let num = num_str.parse::<f64>().map_err(|_| format!("Invalid float: {}", num_str))?;
                tokens.push(Token::Float(num));
            } else {
                let num_str: String = chars[start..i].iter().collect();
                let num = num_str.parse::<i32>().map_err(|_| format!("Invalid integer: {}", num_str))?;
                tokens.push(Token::Integer(num));
            }
            continue;
        }

        // String 'hello'
        if c == '\'' {
            i += 1;
            let start = i;
            while i < chars.len() && chars[i] != '\'' {
                i += 1;
            }
            let s: String = chars[start..i].iter().collect();
            i += 1; // skip closing '
            tokens.push(Token::String(s));
            continue;
        }

        // Symbol #name
        if c == '#' {
            i += 1;
            let start = i;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let s: String = chars[start..i].iter().collect();
            tokens.push(Token::Symbol(s));
            continue;
        }

        // Character $c
        if c == '$' {
            i += 1;
            if i < chars.len() {
                tokens.push(Token::Character(chars[i]));
                i += 1;
            }
            continue;
        }

        // Assignment :=
        if c == ':' && i + 1 < chars.len() && chars[i + 1] == '=' {
            tokens.push(Token::Assign);
            i += 2;
            continue;
        }

        // Delimiters
        match c {
            '(' => { tokens.push(Token::LParen); i += 1; continue; }
            ')' => { tokens.push(Token::RParen); i += 1; continue; }
            '[' => { tokens.push(Token::LBracket); i += 1; continue; }
            ']' => { tokens.push(Token::RBracket); i += 1; continue; }
            '.' => { tokens.push(Token::Period); i += 1; continue; }
            '^' => { tokens.push(Token::Caret); i += 1; continue; }
            '|' => { tokens.push(Token::Pipe); i += 1; continue; }
            ':' => { tokens.push(Token::Colon); i += 1; continue; }
            _ => {}
        }

        // Identifier or keyword
        if c.is_alphabetic() || c == '_' {
            let start = i;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let ident: String = chars[start..i].iter().collect();

            // Check for keyword (ends with :)
            if i < chars.len() && chars[i] == ':' && (i + 1 >= chars.len() || chars[i + 1] != '=') {
                i += 1;
                let keyword = format!("{}:", ident);
                tokens.push(Token::Keyword(keyword));
                continue;
            }

            // Check for reserved words
            match ident.as_str() {
                "true" => tokens.push(Token::True),
                "false" => tokens.push(Token::False),
                "nil" => tokens.push(Token::Nil),
                "self" => tokens.push(Token::Self_),
                _ => tokens.push(Token::Identifier(ident)),
            }
            continue;
        }

        // Binary operators
        if is_binary_char(c) {
            let start = i;
            while i < chars.len() && is_binary_char(chars[i]) {
                i += 1;
            }
            let op: String = chars[start..i].iter().collect();
            tokens.push(Token::BinaryOp(op));
            continue;
        }

        return Err(format!("Unexpected character: '{}' at position {}", c, i));
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

fn is_binary_char(c: char) -> bool {
    matches!(c, '+' | '-' | '*' | '/' | '<' | '>' | '=' | '@' | '%' | '&' | '~' | ',' | '\\')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer() {
        let tokens = tokenize("42").unwrap();
        assert_eq!(tokens[0], Token::Integer(42));
    }

    #[test]
    fn test_string() {
        let tokens = tokenize("'hello'").unwrap();
        assert_eq!(tokens[0], Token::String("hello".to_string()));
    }

    #[test]
    fn test_binary_message() {
        let tokens = tokenize("3 + 4").unwrap();
        assert_eq!(tokens[0], Token::Integer(3));
        assert_eq!(tokens[1], Token::BinaryOp("+".to_string()));
        assert_eq!(tokens[2], Token::Integer(4));
    }

    #[test]
    fn test_keyword_message() {
        let tokens = tokenize("x at: 1 put: 2").unwrap();
        assert_eq!(tokens[0], Token::Identifier("x".to_string()));
        assert_eq!(tokens[1], Token::Keyword("at:".to_string()));
        assert_eq!(tokens[2], Token::Integer(1));
        assert_eq!(tokens[3], Token::Keyword("put:".to_string()));
        assert_eq!(tokens[4], Token::Integer(2));
    }
}
