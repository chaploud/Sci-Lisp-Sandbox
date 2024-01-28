use chrono::{DateTime, Duration, Utc};
use indexmap::{IndexMap, IndexSet};
use num::complex::Complex64;
// regex::Regex
pub trait Any {}
pub trait Callable: Any {}
pub trait Evaluable: Any {}
pub trait Iterable: Any {}
pub trait Collection: Iterable {}

pub struct Nil;
pub struct Bool;
pub struct Regex;
pub struct String;
pub struct Keyword;
pub struct Symbol;
pub enum Number {
    I64(i64),
    F64(f64),
    C64(Complex64),
}
pub struct Datetime;
pub struct TimeDelta;
pub struct Slice;
pub struct List;
pub struct Vector;
pub struct Map;
pub struct Set;
pub struct Function;
pub struct Macro;
pub struct Generator;
pub struct Struct;
pub struct Enum;
pub struct Union;

impl Any for Nil {}
impl Any for Bool {}
impl Any for Regex {}
impl Any for String {}
impl Any for Keyword {}
impl Any for Symbol {}
impl Any for Number {}
impl Any for Datetime {}
impl Any for TimeDelta {}
impl Any for Slice {}
impl Any for List {}
impl Any for Vector {}
impl Any for Map {}
impl Any for Set {}
impl Any for Function {}
impl Any for Macro {}
impl Any for Generator {}
impl Any for Struct {}
impl Any for Enum {}
impl Any for Union {}

impl Callable for Keyword {}
impl Callable for Slice {}
impl Callable for Function {}
impl Callable for Macro {}
impl Callable for String {}
impl Iterable for String {}
impl Iterable for Generator {}
impl Iterable for List {}
impl Iterable for Vector {}
impl Iterable for Map {}
impl Iterable for Set {}
impl Evaluable for Symbol {}
impl Evaluable for List {}
impl Collection for List {}
impl Collection for Vector {}
impl Collection for Map {}
impl Collection for Set {}
