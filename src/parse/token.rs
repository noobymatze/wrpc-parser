use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Data,
    Colon,
    RAngle,
    LAngle,
    Equal,
    Hash,
    Service,
    Enum,
    Def,
    Questionmark,
    Comment(String),
    Identifier(String),
    Symbol(Vec<String>, String),
    String(String),
    Keyword(String),
    Boolean(bool),
    Number(f64),
    Eof,
}
