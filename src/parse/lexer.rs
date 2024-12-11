use crate::error::syntax as error;
use crate::parse::token::Token;
use crate::reporting::Region;

/// A [LexResult] represents the result of trying
/// to lex a group of characters into a [Token].
pub type LexResult = Result<(Region, Token), error::Token>;

/// The current [Context] of the [Lexer].
///
/// See [Lexer] for more details on the meaning
/// and necessity of this.
#[derive(Debug)]
enum Context {
    /// Lexer should operate and tokenize normally.
    Normal,
    /// Lexer should operate and tokenize according to
    /// a subset of [edn](https://github.com/edn-format/edn)
    /// rules. The subset is defined by the implementation. ^^
    Annotation(u32),
}

/// A [Lexer] groups a sequence of characters into [Token]s.
///
/// Since the wRPC language embeds [edn](https://github.com/edn-format/edn)
/// for its annotation meta language and the rules for lexing
/// are different in that context, the lexer switches the
/// state to [Context::Annotation] upon encountering a Hash `#`
/// in [Context::Normal] mode.
///
/// By keeping track of the number of open parenthesis, we
/// know when to exit [Context::Annotation] mode. If the parenthesis
/// are unbalanced, we have a problem, because currently the
/// lexer has no way of being recovered.
struct Lexer<T: Iterator<Item = char>> {
    input: T,
    pending: Vec<(Region, Token)>,
    char0: Option<char>,
    line: usize,
    start_line: usize,
    col: usize,
    start_col: usize,
    ended: bool,
    context: Context,
}

/// Returns a new [`Iterator`] for [`LexResult`]s.
///
/// It can be used to tokenize the given source string.
///
/// ## Example
///
/// Here is an example of how to use it.
///
/// ```ignore
/// let result = lexer("data Person { name: String }").collect::<Vec<LexResult>>()
/// println!("{:?}", result)
/// ```
pub fn lexer(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer::new(input.chars())
}

impl<T: Iterator<Item = char>> Lexer<T> {
    /// Returns a `Lexer` based on the given iterator of characters.
    fn new(input: T) -> Self {
        Lexer {
            start_line: 1,
            start_col: 1,
            line: 1,
            col: 1,
            pending: vec![],
            input,
            char0: None,
            ended: false,
            context: Context::Normal,
        }
    }

    fn consume(&mut self) -> LexResult {
        while self.pending.is_empty() {
            self.consume_next()?;
        }

        Ok(self.pending.remove(0))
    }

    /// Consume the next [`Token`] or [`Error`].
    fn consume_next(&mut self) -> Result<(), error::Token> {
        self.start_line = self.line;
        self.start_col = self.col;
        match self.advance() {
            None => self.emit(Token::Eof),
            Some(c) => self.consume_token(c)?,
        }

        Ok(())
    }

    fn consume_token(&mut self, c: char) -> Result<(), error::Token> {
        match c {
            '\n' => {
                self.line += 1;
                self.col = 1;
            }
            '{' => self.emit(Token::LBrace),
            '}' => self.emit(Token::RBrace),
            ',' => self.emit(Token::Comma),
            '(' => {
                self.emit(Token::LParen);
                // Update the parens to catch when to switch
                // back to normal mode.
                self.context = match self.context {
                    Context::Annotation(n) => Context::Annotation(n + 1),
                    Context::Normal => Context::Normal,
                };
            }
            ')' => {
                self.emit(Token::RParen);
                // If only the opening paren has been
                self.context = match self.context {
                    Context::Annotation(n) if n <= 1 => Context::Normal,
                    Context::Annotation(n) => Context::Annotation(n - 1),
                    Context::Normal => Context::Normal,
                };
            }
            '=' => match self.context {
                Context::Annotation(_) => self.consume_symbol(c)?,
                Context::Normal => self.emit(Token::Equal),
            },
            '>' => match self.context {
                Context::Annotation(_) => self.consume_symbol(c)?,
                Context::Normal => self.emit(Token::RAngle),
            },
            '<' => match self.context {
                Context::Annotation(_) => self.consume_symbol(c)?,
                Context::Normal => self.emit(Token::LAngle),
            },
            ':' => match self.context {
                Context::Normal => self.emit(Token::Colon),
                Context::Annotation(_) => self.consume_keyword(c)?,
            },
            '?' => match self.context {
                Context::Normal => self.emit(Token::Questionmark),
                Context::Annotation(_) => self.consume_symbol(c)?,
            },
            '/' => match self.context {
                Context::Normal => self.consume_comment()?,
                Context::Annotation(_) => self.consume_symbol(c)?,
            },
            '#' => match self.context {
                Context::Normal => {
                    self.context = Context::Annotation(0);
                    self.emit(Token::Hash);
                }
                Context::Annotation(_) => {
                    return Err(error::Token::BadChar(self.line, self.col - 1, c))
                }
            },
            '"' => self.consume_string(c)?,
            c if c.is_whitespace() => {
                // Don't need to do anything here.
            }
            c if c.is_ascii_digit() => self.consume_number(c)?,
            c if c.is_alphabetic() => match self.context {
                Context::Normal => self.consume_identifier(c)?,
                Context::Annotation(_) => self.consume_symbol(c)?,
            },
            '_' => match self.context {
                Context::Normal => self.consume_identifier(c)?,
                Context::Annotation(_) => self.consume_symbol(c)?,
            },
            c if c.is_symbol_start() => match self.context {
                Context::Normal => return Err(error::Token::BadChar(self.line, self.col - 1, c)),
                Context::Annotation(_) => self.consume_symbol(c)?,
            },
            c => return Err(error::Token::BadChar(self.line, self.col - 1, c)),
        }

        Ok(())
    }

    fn consume_number(&mut self, start: char) -> Result<(), error::Token> {
        let mut result = String::from(start);
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            if let Some(c) = self.advance() {
                result.push(c);
            }
        }

        if self.peek() == Some('.') {
            self.advance();
            result.push('.');
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                if let Some(c) = self.advance() {
                    result.push(c);
                }
            }
        }

        let number = result.parse::<f64>().map_err(|err| {
            error::Token::Number(
                self.line,
                self.col - 1,
                error::Number::Bad(format!("{}", err)),
            )
        })?;
        self.emit(Token::Number(number));
        Ok(())
    }

    /// Consume an identifier. An identifier in wrpc starts with
    /// an alphabetic letter.
    fn consume_string(&mut self, _start: char) -> Result<(), error::Token> {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if c == '\\' {
                // Escaped next character
                self.advance();
                match self.advance() {
                    Some('"') => result.push(c),
                    Some('t') => result.push('\t'),
                    Some('n') => result.push('\n'),
                    Some('\\') => result.push('\\'),
                    Some(_) => {
                        return Err(error::Token::String(
                            self.line,
                            self.col,
                            error::Str::StringEscape(error::Escape::EscapeUnknown),
                        ))
                    }
                    None => {
                        return Err(error::Token::String(
                            self.line,
                            self.col,
                            error::Str::Endless,
                        ))
                    }
                }
            } else if c == '"' {
                self.advance();
                break;
            } else {
                self.advance();
                result.push(c);
            }
        }

        self.emit(Token::String(result));

        Ok(())
    }

    /// Consume an identifier. An identifier in wrpc starts with
    /// an alphabetic letter.
    fn consume_keyword(&mut self, _start: char) -> Result<(), error::Token> {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if c.is_symbol() {
                self.advance();
                result.push(c);
            } else {
                break;
            }
        }

        self.emit(Token::Keyword(result));

        Ok(())
    }

    /// Consume an identifier. An identifier in wrpc starts with
    /// an alphabetic letter.
    fn consume_identifier(&mut self, start: char) -> Result<(), error::Token> {
        let mut result = String::from(start);
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
                result.push(c);
            } else {
                break;
            }
        }

        match result.as_str() {
            "data" => self.emit(Token::Data),
            "service" => self.emit(Token::Service),
            "enum" => self.emit(Token::Enum),
            "def" => self.emit(Token::Def),
            _ => self.emit(Token::Identifier(result)),
        };

        Ok(())
    }

    /// Consume an identifier. An identifier in wrpc starts with
    /// an alphabetic letter.
    fn consume_symbol(&mut self, start: char) -> Result<(), error::Token> {
        let mut result = String::from(start);
        while let Some(c) = self.peek() {
            if c.is_symbol() {
                self.advance();
                result.push(c);
            } else {
                break;
            }
        }

        match result.as_str() {
            "true" => self.emit(Token::Boolean(true)),
            "false" => self.emit(Token::Boolean(false)),
            str if str.len() > 1 && str.contains('/') => {
                let vec: Vec<&str> = str.split('/').collect();
                if let Some((a, b)) = vec.split_last() {
                    self.emit(Token::Symbol(
                        b.iter().map(|x| x.to_string()).collect(),
                        a.to_string(),
                    ))
                }
            }
            str => self.emit(Token::Symbol(vec![], str.to_string())),
        }

        Ok(())
    }

    /// Emits a comment [Token].
    ///
    /// A comment is identified by the following rule:
    ///
    /// - Starts with `//` and goes until the end of the line.
    ///
    fn consume_comment(&mut self) -> Result<(), error::Token> {
        if matches!(self.peek(), Some(c) if c != '/') {
            return Err(error::Token::Comment(
                error::Comment::Start,
                self.line,
                self.col,
            ));
        }
        self.advance(); // Consume '/'

        let mut content = String::new();
        while !self.peek().is_none() && !matches!(self.peek(), Some(c) if c == '\n') {
            content.push(self.advance().unwrap())
        }
        self.emit(Token::Comment(content.trim().into()));

        Ok(())
    }

    /// Pushes a new token into the [pending] list of tokens.
    ///
    /// ## Example
    ///
    /// ```ignore
    /// let mut lexer = Lexer::new("test".chars());
    /// assert!(lexer.pending.is_empty());
    /// lexer.emit(Token::Data);
    /// assert!(!lexer.pending.is_empty());
    /// ```
    pub fn emit(&mut self, token: Token) {
        let region = Region::new(
            self.start_line,
            self.start_col,
            self.line,
            self.col - 1, // -1 because the col is always a character further.
        );
        self.pending.push((region, token));
    }

    /// Returns the next character in the input, without advancing
    /// the input.
    ///
    /// This allows to look ahead to decide what to do, based on
    /// the character.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let lexer = Lexer::new("test".chars());
    /// assert_eq!(lexer.peek(), Some('t'));
    /// ```
    pub fn peek(&mut self) -> Option<char> {
        if self.char0.is_none() {
            self.char0 = self.input.next();
        }

        self.char0
    }

    /// Returns the next character in the input.
    ///
    /// If [peek](peek) has been called before, [advance](advance)
    /// will return the peeked character instead of advancing.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let lexer = Lexer::new("test".chars());
    /// assert_eq!(lexer.advance(), Some('t'));
    /// ```
    ///
    /// ```ignore
    /// let lexer = Lexer::new("test".chars());
    /// assert_eq!(lexer.peek(), Some('t'));
    /// assert_eq!(lexer.advance(), Some('t'));
    /// ```
    pub fn advance(&mut self) -> Option<char> {
        self.col += 1;
        match self.char0 {
            None => self.input.next(),
            Some(c) => {
                self.char0 = None;
                Some(c)
            }
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            return None;
        }

        match self.consume() {
            Ok((region, Token::Eof)) => {
                self.ended = true;
                Some(Ok((region, Token::Eof)))
            }
            Ok(token) => Some(Ok(token)),
            Err(err) => Some(Err(err)),
        }
    }
}

trait SymbolExt {
    fn is_symbol_start(&self) -> bool;
    fn is_symbol(&self) -> bool;
    fn is_misc(&self) -> bool;
}

impl SymbolExt for char {
    fn is_symbol_start(&self) -> bool {
        self.is_alphabetic() || self.is_misc()
    }

    fn is_symbol(&self) -> bool {
        self.is_alphanumeric() || self.is_misc()
    }

    fn is_misc(&self) -> bool {
        *self == '*'
            || *self == '.'
            || *self == '!'
            || *self == '-'
            || *self == '_'
            || *self == '?'
            || *self == '$'
            || *self == '%'
            || *self == '&'
            || *self == '='
            || *self == '<'
            || *self == '>'
            || *self == '/'
            || *self == ':'
            || *self == '#'
            || *self == '+'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::error::syntax;
    use crate::parse::lexer::lexer;
    use crate::parse::token::Token;

    #[test]
    fn test_empty_input() {
        let input = "";
        let mut lexer = lexer(input);

        lexer.next();
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_single_identifier() {
        let input = "foo";
        let mut lexer = lexer(input);

        let token = lexer.next();
        assert!(matches!(
            token,
            Some(Ok((_, Token::Identifier(ref id)))) if id == "foo"
        ));
        lexer.next();
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_multiple_identifiers() {
        let input = "foo bar";
        let mut lexer = lexer(input);

        let expected_tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::Identifier("bar".to_string()),
        ];

        for expected in expected_tokens {
            let token = lexer.next();
            assert!(matches!(token, Some(Ok((_, ref t))) if t == &expected));
        }

        lexer.next();
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_mixed_input() {
        let input = "foo (bar)";
        let mut lexer = lexer(input);

        let expected_tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LParen,
            Token::Identifier("bar".to_string()),
            Token::RParen,
            Token::Eof,
        ];

        for expected in expected_tokens {
            let token = lexer.next();
            assert!(matches!(token, Some(Ok((_, ref t))) if t == &expected));
        }

        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_identifier_with_underscores() {
        let input = "foo_bar";
        let mut lexer = lexer(input);

        let token = lexer.next();
        assert!(matches!(
            token,
            Some(Ok((_, Token::Identifier(ref id)))) if id == "foo_bar"
        ));
        lexer.next();
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_unexpected_characters() {
        let input = "foo@";
        let mut lexer = lexer(input);

        let token = lexer.next();
        assert!(matches!(
            token,
            Some(Ok((_, Token::Identifier(ref id)))) if id == "foo"
        ));
        let token = lexer.next();
        assert!(matches!(
            token,
            Some(Err(syntax::Token::BadChar(_, _, '@')))
        ));
    }

    #[test]
    fn test_position_tracking() {
        let input = "foo\nbar";
        let mut lexer = lexer(input);

        let expected_tokens = vec![
            (
                Region::new(1, 1, 1, 3),
                Token::Identifier("foo".to_string()),
            ),
            (
                Region::new(2, 1, 2, 3),
                Token::Identifier("bar".to_string()),
            ),
            (Region::new(2, 4, 2, 4), Token::Eof),
        ];

        for expected in expected_tokens {
            let token = lexer.next();
            assert_eq!(token, Some(Ok(expected)));
        }

        assert!(lexer.next().is_none());
    }
}
