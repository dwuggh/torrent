
pub mod node;
pub mod expr;
pub mod node_to_expr;

#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Copy, Eq)]
pub struct WithSpan<T> {
    content: T,
    span: Span
}
