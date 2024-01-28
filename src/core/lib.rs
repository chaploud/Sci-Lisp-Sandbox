mod rust;

#[no_mangle]
pub extern "C" fn loop_ten_million_times() -> i32 {
    let mut i = 0;
    for _ in 0..10_000_000 {
        i += 1;
    }
    i
}

pub enum Type {
    Nil,
    I64(i64),
}

#[no_mangle]
pub extern "C" fn use_enum() -> i64 {
    let x = Type::I64(42);
    match x {
        Type::Nil => 0,
        Type::I64(s) => s,
    }
}
