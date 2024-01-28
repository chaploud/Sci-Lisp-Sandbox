use std::sync::Arc;

type FunctionType = Arc<dyn Fn(Vec<Arc<dyn Any + 'static>>) -> Arc<dyn Any + 'static> + 'static>;
pub struct Function {
    pub func: FunctionType,
}

pub struct Macro;
pub struct Generator;
pub struct Struct;
pub struct Enum;
pub struct Union;
