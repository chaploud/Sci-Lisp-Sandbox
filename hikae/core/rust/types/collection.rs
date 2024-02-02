use std::any::TypeId;

use crate::rust::types::any::Any;
use crate::rust::types::iterable::Iterable;

pub trait Collection: Iterable + Any {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
}
