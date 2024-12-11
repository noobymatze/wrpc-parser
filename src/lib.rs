use std::path::PathBuf;

use ast::source::Module;
use error::syntax::{self};

pub mod ast;
pub mod error;
pub mod parse;
pub mod reporting;

/// Parses the given [source] into a [Module].
///
/// ## Example
///
/// ```
/// use io::fs;
///
/// let file: PathBuf = "test.wrpc".into();
/// fs::read_to_string(file)
///
/// ```
pub fn parse(filename: Option<PathBuf>, source: &str) -> Result<Module, Vec<syntax::Error>> {
    parse::parse(filename, source)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::parse;

    #[test]
    fn test() -> Result<(), std::io::Error> {
        let content = fs::read_to_string("test.wrpc")?;
        let result = parse(None, content.as_str());
        assert!(result.is_ok());

        Ok(())
    }
}
