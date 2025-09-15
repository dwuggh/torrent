
pub mod node;
pub mod expr;
pub mod node_to_expr;

#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq)]
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
        Self { start: pos, end: pos }
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq)]
pub struct WithSpan<T> {
    content: T,
    span: Span
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
