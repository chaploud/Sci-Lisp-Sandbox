// 必ずshapeを持つ、高速な数値計算用の構造体Array
// 中に格納できる値は、今のところnumberのみ
// 多次元配列を想定

use std::any::TypeId;
use std::sync::Arc;

use ndarray::prelude::*;
use ndarray::Array;

use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::iterable::Iterable;
use crate::rust::types::number::Number;

// TODO:
impl Any for Array<Arc<dyn Number>, IxDyn> {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<Array<Arc<dyn Number>, IxDyn>>()
    }
}
impl Iterable for Array<Arc<dyn Number>, IxDyn> {}
impl Collection for Array<Arc<dyn Number>, IxDyn> {
    fn len(&self) -> usize {
        self.len()
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}
