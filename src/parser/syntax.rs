use std::sync::Arc;

use crate::parser::green::GreenElement;

pub struct SyntaxNode(Arc<SyntaxNodeData>);

pub struct SyntaxNodeData {
    _green: GreenElement,
}
