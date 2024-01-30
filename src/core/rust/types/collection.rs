use crate::rust::types::iterable::Iterable;

pub trait Collection: Iterable {
    fn type_name(&self) -> &'static str {
        "collection"
    }
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
