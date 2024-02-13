use std::{fmt, sync::Arc};

use crate::structures::span::Span;

use super::errors::Error;

// parser -> token & span & structuer
// -> parse_hoge -> node形成(Stack, つながり、箇所、構造) -> ast形成
// stackをポップしていくだけで処理が進む

pub struct AST {
    pub next_node_id: usize,
    pub nodes: Vec<Expr>,
    pub spans: Vec<Span>,
    pub errors: Vec<Error>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    pub id: NodeId,
    pub span: Span,
    pub func: Expr,
    pub args: Vec<Expr>,
}


pub type Expr = Arc<ExprData>;

#[derive(Clone, Debug)]
pub enum ExprData {
    Call(Call),
}
