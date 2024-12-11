use std::path::Path;

use ast::source::Module;
use error::{
    syntax::{self},
    Error,
};
use reporting::WrpcDocBuilder;

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
/// let content = fs::read_to_string("test.wrpc");
/// let result = parse(content.as_str());
/// assert!(result.is_ok());
/// ```
///
/// ```
/// use io::fs;
///
/// let path = "test.wrpc";
/// let content = fs::read_to_string(path)?;
/// match parse(content.as_str()) {
///     Ok(module) => println!("{module:?}"),
///     Err(error) => print_error(path, content.as_str(), error),
/// }
/// ```
pub fn parse(source: &str) -> Result<Module, Error> {
    parse::parse(None, source).map_err(Error::BadSyntax)
}

/// Print the given [error] to the terminal.
pub fn print_error<P: AsRef<Path>>(filename: P, source: &str, error: Error) {
    match error {
        Error::BadSyntax(errors) => {
            let alloc = WrpcDocBuilder::new(source);
            for error in errors {
                match error {
                    syntax::Error::ParseError(error) => {
                        let report = error.to_report(&alloc);
                        println!(
                            "\x1b[31m{}\x1b[0m\n",
                            report.render(
                                &Some(filename.as_ref().to_path_buf()),
                                reporting::Target::Terminal
                            )
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{parse, print_error};

    #[test]
    fn test() -> Result<(), std::io::Error> {
        let path = "test.wrpc";
        let content = fs::read_to_string(path)?;
        match parse(content.as_str()) {
            Ok(module) => println!("{module:?}"),
            Err(error) => print_error(path, content.as_str(), error),
        }

        Ok(())
    }
}
