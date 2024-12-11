use std::borrow::Cow;
use std::ops::Deref;
use std::path::PathBuf;

use pretty::{BoxAllocator, BoxDoc, Pretty};
use serde::ser::SerializeSeq;
use serde::{Deserialize, Serialize, Serializer};

/// A [`Line`] represents a single line in the source input.
pub type Line = usize;

/// A [`Col`] represents a column, meaning a single character in
/// some line in the input.
pub type Col = usize;

/// A [`Position`] represents the exact location of a single character
/// in the source input.
#[derive(Debug, Eq, Ord, PartialOrd, PartialEq, Serialize, Deserialize, Clone)]
pub struct Position {
    pub line: Line,
    pub col: Col,
}

/// A [`Region`] represents a starting and ending [`Position`]
/// in the source input.
#[derive(Debug, Eq, Ord, PartialOrd, PartialEq, Deserialize, Clone)]
pub struct Region {
    pub start: Position,
    pub end: Position,
}

impl Region {
    /// Returns a new `Region`.
    ///
    /// This can be used as a convenience method, to make
    /// creating `Region` more convenient.
    ///
    ///
    /// ## Example
    ///
    /// ```
    /// use compiler::reporting::Region;
    /// let region = Region::new(1, 2, 1, 2);
    /// ```
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Region {
            start: Position {
                line: start_line,
                col: start_col,
            },
            end: Position {
                line: end_line,
                col: end_col,
            },
        }
    }

    /// Returns a new [`Region`], that starts and ends on the same line.
    ///
    /// This is useful for tests, where regions may be on a single line.
    pub fn line(line: usize, start_col: usize, end_col: usize) -> Self {
        Region::new(line, start_col, line, end_col)
    }

    pub fn from_position(start: &Position, end: &Position) -> Self {
        Region {
            start: start.clone(),
            end: end.clone(),
        }
    }
}

impl Serialize for Region {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(4))?;
        seq.serialize_element(&self.start.line)?;
        seq.serialize_element(&self.start.col)?;
        seq.serialize_element(&self.end.line)?;
        seq.serialize_element(&self.end.col)?;
        seq.end()
    }
}

#[derive(Debug)]
pub struct Report<'a> {
    pub title: String,
    pub doc: BoxDoc<'a, ()>,
}

#[derive(Debug)]
pub enum Target {
    _Text,
    Terminal,
}

impl<'a> Report<'a> {
    pub fn render(&self, filename: &Option<PathBuf>, target: Target) -> String {
        let name = filename.as_ref().and_then(|x| x.as_os_str().to_str());
        match target {
            Target::_Text => format!(
                "{}\n\n{}",
                pretty_header(&self.title, name),
                self.doc.deref().pretty(70)
            ),
            Target::Terminal => format!(
                "{}\n\n{}",
                pretty_header(&self.title, name),
                self.doc.deref().pretty(70)
            ),
        }
    }
}

type WrpcDoc<'a> = BoxDoc<'a, ()>;

#[derive(Debug)]
pub struct WrpcDocBuilder<'a> {
    lines: Vec<&'a str>,
}

impl<'a> WrpcDocBuilder<'a> {
    pub fn new(source: &'a str) -> WrpcDocBuilder<'a> {
        let lines = source.lines().collect::<Vec<&str>>();
        WrpcDocBuilder { lines }
    }

    pub fn text<U: Into<Cow<'a, str>>>(&self, text: U) -> WrpcDoc<'a> {
        BoxDoc::text(text)
    }

    pub fn reflow<U: Into<Cow<'a, str>>>(&self, text: U) -> WrpcDoc<'a> {
        let value = text
            .into()
            .split(char::is_whitespace)
            .map(|value| value.to_owned())
            .collect::<Vec<String>>();
        BoxDoc::intersperse(value, BoxDoc::softline())
    }

    pub fn reflow_lines<I>(&self, text: I) -> WrpcDoc<'a>
    where
        I: IntoIterator,
        I::Item: Into<Cow<'a, str>>,
    {
        let mut value = vec![];
        for line in text.into_iter() {
            value.push(self.reflow(line))
        }
        BoxDoc::intersperse(value, BoxDoc::softline())
    }

    pub fn stack<I>(&self, docs: I) -> WrpcDoc<'a>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, BoxAllocator, ()>,
    {
        BoxDoc::intersperse(docs, BoxDoc::line().append(BoxDoc::line()))
    }

    pub fn vcat<I>(&self, docs: I) -> WrpcDoc<'a>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, BoxAllocator, ()>,
    {
        BoxDoc::intersperse(docs, BoxDoc::line())
    }

    pub fn snippet_single(&self, line: Line, col: Col) -> WrpcDoc<'a> {
        self.snippet(&Region::new(line, col, line, col))
    }

    pub fn snippet(&self, region: &Region) -> WrpcDoc<'a> {
        let line = self.lines.get(region.start.line - 1);
        if let Some(line) = line {
            let line_num = format!("{}|  ", region.start.line);
            let line = format!("{}{}", line_num, line);
            let indent = " ".repeat(region.start.col + line_num.len() - 1);
            let values = "^".repeat((region.end.col - region.start.col) + 1);
            let area = format!("{indent}{values}");
            return self.vcat([self.text(line), self.text(area)]);
        }

        BoxDoc::nil()
    }
}

const HEADER_WIDTH: usize = 80;

pub fn pretty_header(title: &str, filename: Option<&str>) -> String {
    let title_width = title.len() + 4;

    if let Some(filename) = filename {
        format!(
            "── {} {} {} ──",
            title,
            "─".repeat(HEADER_WIDTH - title_width - filename.len() - 2),
            filename,
        )
    } else {
        format!("── {} {}", title, "─".repeat(HEADER_WIDTH - title_width))
    }
}
