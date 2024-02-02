use std::any::TypeId;

use chrono::{DateTime, Duration, Utc};

use crate::rust::types::any::Any;

impl Any for DateTime<Utc> {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<DateTime<Utc>>()
    }
}

impl Any for Duration {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<Duration>()
    }
}

impl Any for Utc {
    fn type_id(&self) -> std::any::TypeId {
        TypeId::of::<Utc>()
    }
}
