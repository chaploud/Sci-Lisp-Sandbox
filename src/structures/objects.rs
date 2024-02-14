pub enum Objects {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    List(Vec<Objects>),
    Tuple(Vec<Objects>),
    Dict(Vec<(Objects, Objects)>),
    Function(String, Vec<String>, Vec<Objects>),
    Class(String, Vec<String>, Vec<Objects>),
    Instance(String, Vec<Objects>),
    Module(String, Vec<Objects>),
    None,
}
