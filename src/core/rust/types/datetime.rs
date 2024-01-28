use chrono::{DateTime, Duration, Utc};

use crate::rust::types::any::Any;

impl Any for DateTime<Utc> {}

impl Any for Duration {}

impl Any for Utc {}
