use std::any::TypeId;

use crate::rust::types::any::Any;

impl Any for bool {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<bool>()
    }
}
