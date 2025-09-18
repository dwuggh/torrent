pub mod expr;
pub mod expr_parser;
pub mod macro_expansion;
pub mod scope;
pub mod token;

pub use chumsky::prelude::SimpleSpan;
use chumsky::{
    error::Rich,
    input::{Input, Stream},
    Parser,
};
use logos::Logos;

use crate::core::parser::{expr::Expr, expr_parser::exprs_parser, token::Token};

pub fn parse_src<'s>(src: &'s str) -> Result<Vec<Expr>, Vec<Rich<'s, Token, Span>>> {
    let tokens = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(_) => (Token::Error, span.into()),
    });
    let stream = Stream::from_iter(tokens).map((0..src.len()).into(), |(t, s)| (t, s));
    exprs_parser().parse(stream).into_result()
}

#[derive(Clone, PartialEq, PartialOrd, Copy, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Create a new span with start and end positions
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Create a dummy/empty span at position 0
    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }

    /// Create a span at a single position
    pub fn at(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    /// Get the start position
    pub fn start(&self) -> usize {
        self.start
    }

    /// Get the end position
    pub fn end(&self) -> usize {
        self.end
    }

    /// Get the length of the span
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    /// Check if this span contains a position
    pub fn contains(&self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }

    /// Check if this span overlaps with another span
    pub fn overlaps(&self, other: &Span) -> bool {
        self.start < other.end && other.start < self.end
    }

    /// Merge this span with another span to create a span that covers both
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Extend this span to include a position
    pub fn extend_to(&self, pos: usize) -> Span {
        Span {
            start: self.start.min(pos),
            end: self.end.max(pos + 1),
        }
    }
}

impl chumsky::span::Span for Span {
    type Context = ();

    type Offset = usize;

    fn new(_: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq)]
pub struct WithSpan<T> {
    content: T,
    span: Span,
}

impl<T> WithSpan<T> {
    /// Create a new value with span
    pub fn new(content: T, span: Span) -> Self {
        Self { content, span }
    }

    /// Create a value with a dummy span
    pub fn dummy(content: T) -> Self {
        Self {
            content,
            span: Span::dummy(),
        }
    }

    /// Get a reference to the content
    pub fn content(&self) -> &T {
        &self.content
    }

    /// Get the span
    pub fn span(&self) -> Span {
        self.span
    }

    /// Get a mutable reference to the content
    pub fn content_mut(&mut self) -> &mut T {
        &mut self.content
    }

    /// Consume the wrapper and return the content
    pub fn into_content(self) -> T {
        self.content
    }

    /// Consume the wrapper and return both content and span
    pub fn into_parts(self) -> (T, Span) {
        (self.content, self.span)
    }

    /// Map the content to a new type while preserving the span
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithSpan<U> {
        WithSpan {
            content: f(self.content),
            span: self.span,
        }
    }

    /// Map the content with a function that also receives the span
    pub fn map_with_span<U>(self, f: impl FnOnce(T, Span) -> U) -> WithSpan<U> {
        WithSpan {
            content: f(self.content, self.span),
            span: self.span,
        }
    }

    /// Replace the span while keeping the content
    pub fn with_span(self, span: Span) -> Self {
        Self {
            content: self.content,
            span,
        }
    }
}

impl<T> AsRef<T> for WithSpan<T> {
    fn as_ref(&self) -> &T {
        &self.content
    }
}

impl<T> std::ops::Deref for WithSpan<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}
