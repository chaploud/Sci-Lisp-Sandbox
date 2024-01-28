// 必ずshapeを持つ、高速な数値計算用の構造体Array
// 中に格納できる値は、今のところnumberのみ
// 多次元配列を想定

use std::sync::Arc;

use ndarray::prelude::*;
use ndarray::Array;

use crate::rust::types::any::Any;
use crate::rust::types::collection::Collection;
use crate::rust::types::iterable::Iterable;
use crate::rust::types::number::Number;

impl Any for Array<Arc<dyn Number>, IxDyn> {}
impl Iterable for Array<Arc<dyn Number>, IxDyn> {}
impl Collection for Array<Arc<dyn Number>, IxDyn> {}
