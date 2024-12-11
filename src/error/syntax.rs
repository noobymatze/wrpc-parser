/// ! This module contains all potential syntax errors
/// ! of the wRPC language.
use serde::{Deserialize, Serialize};

use crate::ast::source as ast;
use crate::parse::token;
use crate::reporting::Region;
use crate::reporting::{Col, Line, Position, Report, WrpcDocBuilder};

/// A convenience [`Result`][Result] for working with
/// syntax errors.
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Error {
    ParseError(Module),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Module {
    Decl(Decl),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Decl {
    BadStart(Line, Col),
    BadData(Data),
    BadService(Service),
    BadEnum(Enum),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Data {
    BadName(Name),
    BadComment(Token),
    BadProperty(Property),
    BadType(Type),
    MissingStart(Line, Col),
    MissingEnd(ast::Name, usize, usize),
    BadAnnotation(Annotation),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Enum {
    BadName(Name),
    BadComment(Token),
    BadVariant(Variant),
    BadType(Type),
    MissingStart(Line, Col),
    MissingEnd(usize, usize),
    BadAnnotation(Annotation),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Service {
    BadName(Name),
    BadMethod(Method),
    MissingStart(Line, Col),
    MissingEnd(ast::Name, usize, usize),
    BadAnnotation(Annotation),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Variant {
    BadName(Name),
    BadComment(Token),
    BadAnnotation(Annotation),
    BadProperty(Property),
    BadReturnType(Type),
    MissingComma(Line, Col),
    MissingParamStart(ast::Name, Line, Col),
    MissingParamEnd(ast::Name, Line, Col),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Method {
    BadName(Name),
    BadComment(Token),
    BadParam(Property),
    BadReturnType(Type),
    MissingDef(Line, Col),
    MissingParamStart(ast::Name, Line, Col),
    MissingParamEnd(ast::Name, Line, Col),
    BadAnnotation(Annotation),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Annotation {
    BadExpr(Expr),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Type {
    BadName(Name),
    MissingComma(Position),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Property {
    BadName(Name),
    BadType(ast::Name, Type),
    BadComment(Token),
    MissingComma(Line, Col),
    MissingType(Region),
    MissingColon(ast::Name, Line, Col),
    BadAnnotation(Annotation),
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Token {
    String(Line, Col, Str),
    Number(Line, Col, Number),
    Comment(Comment, Line, Col),
    BadChar(Line, Col, char),
    Eof(Line, Col),
}

impl Token {
    pub fn position(&self) -> Position {
        let (line, col) = match self {
            Token::String(line, col, _) => (line, col),
            Token::Number(line, col, _) => (line, col),
            Token::Comment(_, line, col) => (line, col),
            Token::BadChar(line, col, _) => (line, col),
            Token::Eof(line, col) => (line, col),
        };

        Position {
            line: *line,
            col: *col,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Name {
    BadToken(Token),
    ExpectedName(Line, Col),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Expr {
    String(Str, Line, Col),
    Number(Number, Line, Col),
    Endless(Line, Col),
    Unexpected(Region, token::Token),
    BadToken(Token),
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Str {
    Endless,
    StringEscape(Escape),
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Comment {
    Start,
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Number {
    Bad(String),
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum Escape {
    EscapeUnknown,
}

impl Error {
    pub fn to_report<'a>(&self, alloc: &'a WrpcDocBuilder) -> Report<'a> {
        match self {
            Error::ParseError(module) => module.to_report(alloc),
        }
    }
}

impl Module {
    pub fn to_report<'a>(&self, alloc: &'a WrpcDocBuilder) -> Report<'a> {
        match self {
            Module::Decl(decl) => decl.to_report(alloc),
        }
    }
}

impl Decl {
    pub fn to_report<'a>(&self, alloc: &'a WrpcDocBuilder) -> Report<'a> {
        match self {
            Decl::BadStart(line, col) => Report {
                title: "DECL START".to_owned(),
                doc: alloc.stack([
                    alloc.reflow("I tried to parse either a `Data`, `Service` or `Enum` declaration, but got stuck here:"),
                    alloc.vcat([
                        alloc.snippet_single(*line, *col),
                        alloc.reflow_lines([
                            "You can start a declaration with `data`, `service` or `enum` ",
                            "respectively. Here is an example of how to define a data declaration:"
                        ]),
                    ]),
                    alloc.text(r#">  // This is a doc comment in *markdown*
>  data Foo {
>      name: String,
>  }"#)
                ]),
            },
            Decl::BadData(Data::BadComment(_)) => Report {
                title: "COMMENT SYNTAX".to_owned(),
                doc: alloc.stack([alloc.reflow("Test"), alloc.reflow("Mehr Test")]),
            },
            Decl::BadData(Data::MissingEnd(name, line, col))
            | Decl::BadService(Service::MissingEnd(name, line, col)) => Report {
                title: "UNEXPECTED END OF DECLARATION".to_owned(),
                doc: alloc.stack([
                    alloc.reflow(
                        format!("I was just parsing the `{}` declaration, but missed a closing }}.", name.value),
                    ),
                    alloc.vcat([
                        alloc.snippet_single(*line, *col),
                        alloc.reflow("Please add a closing }."),
                    ])
                ]),
            },
            Decl::BadData(Data::MissingStart(line, col)) => Report {
                title: "DATA DECLARATION".to_owned(),
                doc: alloc.stack([
                    alloc.reflow("I tried to read a data declaration"),
                    alloc.snippet(&Region::new(*line, *col, *line, *col)),
                ]),
            },
            Decl::BadData(Data::BadName(Name::BadToken(token))) => Report {
                title: "DATA DECLARATION".to_owned(),
                doc: alloc.stack([
                    alloc.reflow("I found a token, that I could not understand. "),
                    alloc.reflow(format!("{:?}", token)),
                    ]),
            },

            Decl::BadData(Data::BadName(Name::ExpectedName(line, col))) => Report {
                title: "DATA DECLARATION".to_owned(),
                doc: alloc.stack([
                    alloc.reflow("I expected a name here"),
                    alloc.snippet_single(*line, *col),
                ]),
            },
            Decl::BadData(Data::BadProperty(Property::MissingComma(line, col))) => Report {
                title: "MISSING PROPERTY NAME".to_string(),
                //region: region.clone(),
                doc: alloc.stack([
                    alloc.reflow("I am missing a comma in a property declaration:"),
                    alloc.snippet(&Region::line(*line, *col, *col)),
                ]),
            },
            Decl::BadData(Data::BadProperty(Property::BadType(prop_name, Type::BadName(Name::ExpectedName(line, col))))) => Report {
                title: "BAD PROPERTY DECLARATION".to_string(),
                //region: region.clone(),
                doc: alloc.stack([
                    alloc.reflow(format!("I was just reading a property declaration of `{}` but could not find its type.", prop_name.value)),
                    alloc.snippet(&Region::line(*line, *col, *col)),
                    alloc.reflow("A property declaration should be declared like: name: Type"),
                ]),
            },
            //Decl::MissingPropertySeparator(region) => Report {
            //    title: "MISSING PROPERTY SEPARATOR".to_string(),
            //    doc: alloc.stack([
            //        alloc.reflow("I missed a separator between two properties."),
            //        alloc.snippet(region),
            //        alloc.reflow("Properties can be declared in the form of `name: Type,`. Please add a comma.")
            //    ])
            //},
            Decl::BadData(Data::BadProperty(Property::MissingColon(_name, line, col))) => Report {
                title: "MISSING PROPERTY NAME AND TYPE SEPARATOR".to_string(),
                doc: alloc.stack([
                    alloc.reflow(format!("I found a property with the name `{}`", "Test")),
                    alloc.snippet(&Region::line(*line, *col, *col)),
                ]),
            },
            Decl::BadData(Data::BadProperty(Property::MissingType(region))) => Report {
                title: "MISSING PROPERTY TYPE".to_string(),
                doc: alloc.stack([
                    alloc.reflow(format!(
                        "I found a property with the name `{}`, but \
                             cannot find a type associated with this property.",
                        "Test",
                    )),
                    alloc.snippet(region),
                ]),
            },
            Decl::BadData(Data::BadAnnotation(annotation)) => Report {
                title: "MISSING PROPERTY NAME AND TYPE SEPARATOR".to_string(),
                doc: alloc.stack([
                    alloc.reflow(format!("I found a bad annotation: `{annotation:?}`")),
                ]),
            },
            Decl::BadData(data) => Report {
                title: "BAD DATA DECLARATION".to_string(),
                //region: region.clone(),
                doc: alloc.stack([alloc.reflow(format!("The following error occurred: {:?}", data))]),
            },
            Decl::BadService(_) => Report {
                title: "BAD SERVICE DECLARATION".to_owned(),
                doc: alloc.stack([alloc.reflow("TEST SERVICE")]),
            },
            Decl::BadEnum(_) => Report {
                title: "BAD ENUM DECLARATION".to_owned(),
                doc: alloc.stack([alloc.reflow("TEST ENUM")]),
            },
        }
    }
}
