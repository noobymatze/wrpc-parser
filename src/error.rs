/// ! Represents all errors happening in this project.
use serde::{Deserialize, Serialize};

pub mod syntax;

#[derive(Debug, Serialize, Deserialize)]
pub enum Error {
    BadSyntax(Vec<syntax::Error>),
}
