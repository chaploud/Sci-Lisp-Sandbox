use chrono::{DateTime, Duration, Utc};

use crate::rust::types::any::Any;

impl Any for DateTime<Utc> {
    fn type_name(&self) -> &'static str {
        "datetime"
    }
}

impl Any for Duration {
    fn type_name(&self) -> &'static str {
        "duration"
    }
}

impl Any for Utc {
    fn type_name(&self) -> &'static str {
        "utc"
    }
}
