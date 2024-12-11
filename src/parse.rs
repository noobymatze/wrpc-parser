use crate::ast::source::{
    Annotation, Data, Decl, Enum, Expr, Method, Module, Name, Parameter, Property, Service, Type,
    Variant,
};
use crate::error::syntax;
use crate::parse::lexer::LexResult;
use crate::parse::token::Token;
use crate::reporting::{Position, Region};
use std::path::PathBuf;
use std::vec;

pub mod lexer;
pub mod token;

pub fn parse(_filename: Option<PathBuf>, source: &str) -> Result<Module, Vec<syntax::Error>> {
    let tokenizer = lexer::lexer(source);
    let mut parser = Parser::new(tokenizer);
    parser.parse_module()
}

#[derive(Debug)]
struct Parser<T: Iterator<Item = LexResult>> {
    input: T,
    token1: Option<Result<(Region, Token), syntax::Token>>,
    errors: Vec<syntax::Error>,
    last_position: Position,
}

impl<T> Parser<T>
where
    T: Iterator<Item = LexResult>,
{
    fn new(input: T) -> Self {
        Parser {
            input,
            token1: None,
            errors: vec![],
            last_position: Position { line: 0, col: 0 },
        }
    }

    fn parse_module(&mut self) -> Result<Module, Vec<syntax::Error>> {
        let mut declarations: Vec<Decl> = vec![];

        loop {
            let next = self.parse_decl();
            match next {
                Ok(None) => break,
                Ok(Some(decl)) => declarations.push(decl),
                Err(error) => {
                    self.errors
                        .push(syntax::Error::ParseError(syntax::Module::Decl(error)));
                    self.recover();
                }
            }
        }

        let errors = &self.errors;
        if !errors.is_empty() {
            Err(errors.clone()) // There is probably a better way, then to clone the whole thing
        } else {
            Ok(Module {
                version: "1".into(),
                declarations,
                doc_comment: None,
            })
        }
    }

    fn parse_decl(&mut self) -> Result<Option<Decl>, syntax::Decl> {
        let comment = self
            .parse_comment()
            .map_err(|error| syntax::Decl::BadData(syntax::Data::BadComment(error)))?;
        let annotations = self
            .parse_annotations()
            .map_err(|error| syntax::Decl::BadData(syntax::Data::BadAnnotation(error)))?;
        match self.advance() {
            None => Ok(None),
            Some(Ok((_, Token::Data))) => self
                .parse_data(comment, annotations)
                .map(|x| Some(Decl::Data(x)))
                .map_err(syntax::Decl::BadData),
            Some(Ok((_, Token::Service))) => self
                .parse_service(comment, annotations)
                .map(|x| Some(Decl::Service(x)))
                .map_err(syntax::Decl::BadService),
            Some(Ok((_, Token::Enum))) => self
                .parse_enum(comment, annotations)
                .map(|x| Some(Decl::Enum(x)))
                .map_err(syntax::Decl::BadEnum),
            Some(Ok((region, _))) => {
                Err(syntax::Decl::BadStart(region.start.line, region.start.col))
            }
            Some(Err(_)) => Err(syntax::Decl::BadStart(0, 0)),
        }
    }

    fn recover(&mut self) {
        while !matches!(
            self.peek(),
            Some(Token::Data) | Some(Token::Service) | Some(Token::Enum) | Some(Token::Eof) | None
        ) {
            self.advance();
        }
    }

    fn parse_enum(
        &mut self,
        comment: Option<String>,
        annotations: Vec<Annotation>,
    ) -> Result<Enum, syntax::Enum> {
        let name = self.expect_name().map_err(syntax::Enum::BadName)?;
        let type_variables = self.parse_type_variable(syntax::Enum::BadType)?;
        self.expect_token(Token::LBrace, |pos| {
            syntax::Enum::MissingStart(pos.line, pos.col)
        })?;

        let variants = self.parse_variants().map_err(syntax::Enum::BadVariant)?;
        self.expect_token(Token::RBrace, |pos| {
            syntax::Enum::MissingEnd(pos.line, pos.col)
        })?;

        Ok(Enum {
            annotations,
            doc_comment: comment,
            name,
            variants,
            type_variables,
        })
    }

    fn parse_variants(&mut self) -> Result<Vec<Variant>, syntax::Variant> {
        let mut variants = vec![];
        while !matches!(self.peek(), Some(Token::RBrace) | Some(Token::Eof) | None) {
            self.parse_variant(&mut variants)?;
            if !matches!(self.peek(), Some(Token::RBrace)) {
                self.expect_token(Token::Comma, |pos| {
                    syntax::Variant::MissingComma(pos.line, pos.col)
                })?;
            }
        }

        Ok(variants)
    }

    fn parse_variant(&mut self, variants: &mut Vec<Variant>) -> Result<(), syntax::Variant> {
        let comment = self.parse_comment().map_err(syntax::Variant::BadComment)?;
        let annotations = self
            .parse_annotations()
            .map_err(syntax::Variant::BadAnnotation)?;
        let name = self.expect_name().map_err(syntax::Variant::BadName)?;
        let properties = if self.matches(Token::LBrace) {
            let properties = self
                .parse_properties()
                .map_err(syntax::Variant::BadProperty)?;

            self.expect_token(Token::RBrace, |pos| {
                syntax::Variant::MissingParamEnd(name.clone(), pos.line, pos.col)
            })?;

            properties
        } else {
            vec![]
        };

        let variant = Variant {
            name,
            properties,
            annotations,
            doc_comment: comment,
        };

        variants.push(variant);

        Ok(())
    }

    fn parse_service(
        &mut self,
        comment: Option<String>,
        annotations: Vec<Annotation>,
    ) -> Result<Service, syntax::Service> {
        let name = self.expect_name().map_err(syntax::Service::BadName)?;
        self.expect_token(Token::LBrace, |pos| {
            syntax::Service::MissingStart(pos.line, pos.col)
        })?;

        let methods = self.parse_methods().map_err(syntax::Service::BadMethod)?;
        self.expect_token(Token::RBrace, |pos| {
            syntax::Service::MissingEnd(name.clone(), pos.line, pos.col)
        })?;

        Ok(Service {
            annotations,
            doc_comment: comment,
            name,
            methods,
        })
    }

    fn parse_methods(&mut self) -> Result<Vec<Method>, syntax::Method> {
        let mut methods = vec![];
        while !matches!(self.peek(), Some(Token::RBrace) | Some(Token::Eof) | None) {
            self.parse_method(&mut methods)?;
        }

        Ok(methods)
    }

    fn parse_method(&mut self, methods: &mut Vec<Method>) -> Result<(), syntax::Method> {
        let comment = self.parse_comment().map_err(syntax::Method::BadComment)?;
        let annotations = self
            .parse_annotations()
            .map_err(syntax::Method::BadAnnotation)?;
        self.expect_token(Token::Def, |pos| {
            syntax::Method::MissingDef(pos.line, pos.col)
        })?;
        let name = self.expect_name().map_err(syntax::Method::BadName)?;
        self.expect_token(Token::LParen, |pos| {
            syntax::Method::MissingParamStart(name.clone(), pos.line, pos.col)
        })?;
        let properties = self.parse_properties().map_err(syntax::Method::BadParam)?;

        self.expect_token(Token::RParen, |pos| {
            syntax::Method::MissingParamEnd(name.clone(), pos.line, pos.col)
        })?;

        let return_type = if self.matches(Token::Colon) {
            Some(self.parse_type().map_err(syntax::Method::BadReturnType)?)
        } else {
            None
        };

        let method = Method {
            name,
            parameters: properties
                .iter()
                .map(|prop| Parameter {
                    name: prop.name.clone(),
                    annotations: prop.annotations.clone(),
                    type_: prop.type_.clone(),
                })
                .collect(),
            annotations,
            return_type,
            doc_comment: comment,
        };

        methods.push(method);

        Ok(())
    }

    fn parse_data(
        &mut self,
        comment: Option<String>,
        annotations: Vec<Annotation>,
    ) -> Result<Data, syntax::Data> {
        let name = self.expect_name().map_err(syntax::Data::BadName)?;
        let type_variables = self.parse_type_variable(syntax::Data::BadType)?;
        let mut properties = vec![];
        if self.matches(Token::LBrace) {
            let mut parsed_properties =
                self.parse_properties().map_err(syntax::Data::BadProperty)?;
            properties.append(&mut parsed_properties);
            self.expect_token(Token::RBrace, |pos| {
                syntax::Data::MissingEnd(name.clone(), pos.line, pos.col)
            })?;
        }

        Ok(Data {
            annotations,
            doc_comment: comment,
            name,
            properties,
            type_variables,
        })
    }

    fn parse_properties(&mut self) -> Result<Vec<Property>, syntax::Property> {
        let mut properties = vec![];
        while self.matches_property_start() {
            self.parse_property(&mut properties)?;
            if !matches!(self.peek(), Some(Token::RBrace) | Some(Token::RParen)) {
                self.expect_token(Token::Comma, |pos| {
                    syntax::Property::MissingComma(pos.line, pos.col)
                })?;
            }
        }

        Ok(properties)
    }

    fn parse_property(&mut self, properties: &mut Vec<Property>) -> Result<(), syntax::Property> {
        let comment = self.parse_comment().map_err(syntax::Property::BadComment)?;
        let annotations = self
            .parse_annotations()
            .map_err(syntax::Property::BadAnnotation)?;
        if self.matches_property_start() {
            let name = self.expect_name().map_err(syntax::Property::BadName)?;
            self.expect_token(Token::Colon, |pos| {
                syntax::Property::MissingColon(name.clone(), pos.line, pos.col)
            })?;

            let type_ = self
                .parse_type()
                .map_err(|error| syntax::Property::BadType(name.clone(), error))?;

            let property = Property {
                name,
                type_,
                annotations,
                doc_comment: comment,
            };
            properties.push(property);
        }

        Ok(())
    }

    fn matches_property_start(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token::Identifier(_))
                | Some(Token::Comment(_))
                | Some(Token::Service)
                | Some(Token::Hash)
                | Some(Token::Data)
                | Some(Token::Def)
                | Some(Token::Enum)
        )
    }

    fn parse_comment(&mut self) -> Result<Option<String>, syntax::Token> {
        let mut lines = vec![];
        while let Some(Token::Comment(comment)) = self.peek() {
            lines.push(comment.trim().to_string());
            self.advance();
        }

        if lines.is_empty() {
            Ok(None)
        } else {
            Ok(Some(lines.join("\n")))
        }
    }

    fn parse_type(&mut self) -> Result<Type, syntax::Type> {
        let name = self.expect_name().map_err(syntax::Type::BadName)?;
        let mut variables = vec![];
        if self.matches(Token::LAngle) {
            while !self.matches(Token::RAngle) {
                variables.push(self.parse_type()?);
                if !matches!(self.peek(), Some(Token::RAngle)) {
                    self.expect_token(Token::Comma, syntax::Type::MissingComma)?;
                }
            }
        }
        let type_ = Type { name, variables };
        let questionmark = self.optional(Token::Questionmark);
        if let Some((region, _)) = questionmark {
            Ok(Type {
                name: Name {
                    region,
                    value: "Option".to_string(),
                },
                variables: vec![type_],
            })
        } else {
            Ok(type_)
        }
    }

    fn parse_type_variable<F, E>(&mut self, map_err: F) -> Result<Vec<Name>, E>
    where
        F: Fn(syntax::Type) -> E,
    {
        let mut variables = vec![];
        if self.matches(Token::LAngle) {
            while !self.matches(Token::RAngle) {
                let name = self
                    .expect_name()
                    .map_err(|err| map_err(syntax::Type::BadName(err)))?;
                variables.push(name);
                if !matches!(self.peek(), Some(Token::RAngle)) {
                    self.expect_token(Token::Comma, |pos| {
                        map_err(syntax::Type::MissingComma(pos))
                    })?;
                }
            }
        }

        Ok(variables)
    }

    fn parse_annotations(&mut self) -> Result<Vec<Annotation>, syntax::Annotation> {
        let mut annotations = vec![];
        while self.matches(Token::Hash) {
            let expr = self.parse_expr().map_err(syntax::Annotation::BadExpr)?;
            annotations.push(Annotation { expr })
        }

        Ok(annotations)
    }

    fn parse_expr(&mut self) -> Result<Expr, syntax::Expr> {
        let expr = match self.advance() {
            None => {
                return Err(syntax::Expr::BadToken(syntax::Token::Eof(
                    self.last_position.line,
                    self.last_position.col,
                )))
            }
            Some(Ok((region, Token::String(value)))) => Expr::String(region, value),
            Some(Ok((region, Token::Number(value)))) => Expr::Number(region, value),
            Some(Ok((region, Token::Boolean(value)))) => Expr::Boolean(region, value),
            Some(Ok((region, Token::Symbol(_, value)))) => Expr::Symbol(region, value),
            Some(Ok((region, Token::Keyword(value)))) => Expr::Keyword(region, value),
            Some(Ok((region, Token::Identifier(value)))) => Expr::Symbol(region, value),
            Some(Ok((region, Token::LAngle))) => Expr::Symbol(region, "<".into()),
            Some(Ok((region, Token::RAngle))) => Expr::Symbol(region, ">".into()),
            Some(Ok((region, Token::Data))) => Expr::Symbol(region, "data".into()),
            Some(Ok((region, Token::Service))) => Expr::Symbol(region, "service".into()),
            Some(Ok((region, Token::Enum))) => Expr::Symbol(region, "enum".into()),
            Some(Ok((region, Token::LParen))) => {
                let mut expressions = vec![];
                while !self.matches(Token::RParen) {
                    expressions.push(self.parse_expr()?);
                }

                Expr::List(region, expressions)
            }
            Some(Ok((region, Token::LBrace))) => {
                let mut expressions = vec![];
                while !self.matches(Token::RBrace) {
                    let left = self.parse_expr()?;
                    let right = self.parse_expr()?;
                    expressions.push((left, right));
                }

                Expr::Map(region, expressions)
            }
            Some(Ok((region, token))) => return Err(syntax::Expr::Unexpected(region, token)),
            Some(Err(token)) => return Err(syntax::Expr::BadToken(token)),
        };

        Ok(expr)
    }

    // HELPERS

    //fn check(&mut self, expected: Token) -> bool {
    //    matches!(self.peek(), Some(token) if token == &expected)
    //}

    fn matches(&mut self, expected: Token) -> bool {
        if matches!(self.peek(), Some(token) if token == &expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn optional(&mut self, expected: Token) -> Option<(Region, Token)> {
        match self.peek() {
            Some(token) if token == &expected => self
                .advance()
                .map(|x| x.expect("Since it is a token, there cannot be any error")),
            _ => None,
        }
    }

    fn expect_token<F, E>(&mut self, token: Token, err: F) -> Result<(), E>
    where
        F: Fn(Position) -> E,
    {
        match self.advance() {
            Some(Ok((_, tok))) if tok == token => Ok(()),
            Some(Ok((region, _))) => Err(err(region.end)),
            Some(Err(bad_token)) => Err(err(bad_token.position())),
            None => Err(err(self.last_position.clone())),
        }
    }

    fn expect_name(&mut self) -> Result<Name, syntax::Name> {
        match self.advance() {
            Some(Ok((region, Token::Identifier(name)))) => Ok(Name {
                region,
                value: name,
            }),
            Some(Ok((region, Token::Data))) => Ok(Name {
                region,
                value: "data".to_string(),
            }),
            Some(Ok((region, Token::Service))) => Ok(Name {
                region,
                value: "service".to_string(),
            }),
            Some(Ok((region, Token::Def))) => Ok(Name {
                region,
                value: "def".to_string(),
            }),
            Some(Ok((region, Token::Enum))) => Ok(Name {
                region,
                value: "enum".to_string(),
            }),
            Some(Ok((region, _))) => Err(syntax::Name::ExpectedName(
                region.start.line,
                region.start.col,
            )),
            Some(Err(bad_token)) => Err(syntax::Name::BadToken(bad_token)),
            None => Err(syntax::Name::BadToken(syntax::Token::Eof(
                self.last_position.line,
                self.last_position.col,
            ))),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.next_token();
        self.token1
            .as_ref()
            .and_then(|x| x.as_ref().ok().map(|(_, token)| token))
    }

    fn advance(&mut self) -> Option<Result<(Region, Token), syntax::Token>> {
        self.next_token();
        match self.token1.take() {
            None => None,
            Some(Ok((region, Token::Eof))) => {
                self.last_position = region.end.clone();
                None
            }
            Some(value) => Some(value),
        }
    }

    fn next_token(&mut self) {
        if self.token1.is_none() {
            self.token1 = self.input.next();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data_decl_without_braces_is_ok() {
        let result = parse(None, "data Test");
        assert!(result.is_ok())
    }

    #[test]
    fn test_data_decl_with_missing_ending_brace_errors() {
        let result = parse(None, "data Test {");
        assert!(result.is_err())
    }

    #[test]
    fn test_data_decl_with_ending_brace_is_ok() {
        let result = parse(None, "data Test {}");
        assert!(result.is_ok())
    }

    // Helper function to create a lexer from a source string
    fn create_lexer(source: &str) -> impl Iterator<Item = LexResult> + '_ {
        lexer::lexer(source)
    }

    #[test]
    fn test_parse_empty_module() {
        let source = "";
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert!(module.declarations.is_empty());
    }

    #[test]
    fn test_parse_data_declaration() {
        let source = r#"
            // Hallo Welt!
            data Greet {
                // Test
                // Multiline Test
                name: String,
                // Multiline Test
                test: Boolean,
            }
            "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
        match &module.declarations[0] {
            Decl::Data(data) => {
                assert_eq!(data.name.value, "Greet");
                assert_eq!(data.properties.len(), 2);
                assert_eq!(data.properties[0].name.value, "name");
                assert_eq!(data.properties[0].type_.name.value, "String");
                assert_eq!(data.properties[1].name.value, "test");
                assert_eq!(data.properties[1].type_.name.value, "Boolean");
            }
            _ => panic!("Expected data declaration"),
        }
    }

    #[test]
    fn test_parse_service_declaration() {
        let source = r#"
            service Foo {
                def foo(name: String): Greet
            }
            "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
        match &module.declarations[0] {
            Decl::Service(service) => {
                assert_eq!(service.name.value, "Foo");
                assert_eq!(service.methods.len(), 1);
                assert_eq!(service.methods[0].name.value, "foo");
                assert_eq!(service.methods[0].parameters.len(), 1);
                assert_eq!(service.methods[0].parameters[0].name.value, "name");
                assert_eq!(service.methods[0].parameters[0].type_.name.value, "String");
                assert!(service.methods[0].return_type.is_some());
                assert_eq!(
                    service.methods[0].return_type.as_ref().unwrap().name.value,
                    "Greet"
                );
            }
            _ => panic!("Expected service declaration"),
        }
    }

    #[test]
    fn test_parse_module_with_comments() {
        let source = r#"
            // This is a comment
            data TestData {
                field1: String,
                field2: Integer,
            }

            // Another comment
            service TestService {
                def testMethod(param: String): TestData
            }
            "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 2);

        match &module.declarations[0] {
            Decl::Data(data) => {
                assert_eq!(data.name.value, "TestData");
                assert_eq!(data.properties.len(), 2);
                assert_eq!(data.properties[0].name.value, "field1");
                assert_eq!(data.properties[0].type_.name.value, "String");
                assert_eq!(data.properties[1].name.value, "field2");
                assert_eq!(data.properties[1].type_.name.value, "Integer");
            }
            _ => panic!("Expected data declaration"),
        }

        match &module.declarations[1] {
            Decl::Service(service) => {
                assert_eq!(service.name.value, "TestService");
                assert_eq!(service.methods.len(), 1);
                assert_eq!(service.methods[0].name.value, "testMethod");
                assert_eq!(service.methods[0].parameters.len(), 1);
                assert_eq!(service.methods[0].parameters[0].name.value, "param");
                assert_eq!(service.methods[0].parameters[0].type_.name.value, "String");
                assert!(service.methods[0].return_type.is_some());
                assert_eq!(
                    service.methods[0].return_type.as_ref().unwrap().name.value,
                    "TestData"
                );
            }
            _ => panic!("Expected service declaration"),
        }
    }

    #[test]
    fn test_parse_module_with_syntax_errors() {
        let source = r#"
            data IncorrectData {
                name: String
                test: Boolean, // Missing comma between fields
            }

            service IncorrectService {
                def incorrectMethod(param String) // Missing colon before type
            }
            "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn test_empty_data_block() {
        let source = r#"
           data EmptyData {}
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
        match &module.declarations[0] {
            Decl::Data(data) => {
                assert_eq!(data.name.value, "EmptyData");
                assert!(data.properties.is_empty());
            }
            _ => panic!("Expected data declaration"),
        }
    }

    #[test]
    fn test_empty_service_block() {
        let source = r#"
           service EmptyService {}
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
        match &module.declarations[0] {
            Decl::Service(service) => {
                assert_eq!(service.name.value, "EmptyService");
                assert!(service.methods.is_empty());
            }
            _ => panic!("Expected service declaration"),
        }
    }

    #[test]
    fn test_malformed_input_missing_braces() {
        let source = r#"
           data MissingBraces
               name: String,
               test: Boolean,
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_comments_only() {
        let source = r#"
           // This is a comment
           // Another comment
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert!(module.declarations.is_empty());
    }

    #[test]
    fn test_unusual_identifiers() {
        let source = r#"
           data Data123 {
               _fieldName: String,
               field_with_number1: Integer,
           }
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
        match &module.declarations[0] {
            Decl::Data(data) => {
                assert_eq!(data.name.value, "Data123");
                assert_eq!(data.properties.len(), 2);
                assert_eq!(data.properties[0].name.value, "_fieldName");
                assert_eq!(data.properties[0].type_.name.value, "String");
                assert_eq!(data.properties[1].name.value, "field_with_number1");
                assert_eq!(data.properties[1].type_.name.value, "Integer");
            }
            _ => panic!("Expected data declaration"),
        }
    }

    #[test]
    fn test_whitespace_variations() {
        let source = r#"
           data TestData      {
               name   :     String ,
               test : Boolean ,
           }

           service TestService
           {
               def foo( name   :  String   ) : Greet
           }
           "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 2);

        match &module.declarations[0] {
            Decl::Data(data) => {
                assert_eq!(data.name.value, "TestData");
                assert_eq!(data.properties.len(), 2);
                assert_eq!(data.properties[0].name.value, "name");
                assert_eq!(data.properties[0].type_.name.value, "String");
                assert_eq!(data.properties[1].name.value, "test");
                assert_eq!(data.properties[1].type_.name.value, "Boolean");
            }
            _ => panic!("Expected data declaration"),
        }

        match &module.declarations[1] {
            Decl::Service(service) => {
                assert_eq!(service.name.value, "TestService");
                assert_eq!(service.methods.len(), 1);
                assert_eq!(service.methods[0].name.value, "foo");
                assert_eq!(service.methods[0].parameters.len(), 1);
                assert_eq!(service.methods[0].parameters[0].name.value, "name");
                assert_eq!(service.methods[0].parameters[0].type_.name.value, "String");
                assert!(service.methods[0].return_type.is_some());
                assert_eq!(
                    service.methods[0].return_type.as_ref().unwrap().name.value,
                    "Greet"
                );
            }
            _ => panic!("Expected service declaration"),
        }
    }

    #[test]
    fn test_unexpected_tokens() {
        let source = r#"
       data TestData {
           name: String,
           123invalid: Boolean,
       }
       "#;
        let lexer = create_lexer(source);
        let mut parser = Parser::new(lexer);
        let result = parser.parse_module();

        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
    }
}
