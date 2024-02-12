use std::{fmt, sync::Arc};

use crate::structures::span::Span;

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
