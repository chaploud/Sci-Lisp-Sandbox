mod rust;

#[no_mangle]
pub extern "C" fn loop_ten_million_times() -> i32 {
    let mut i = 0;
    for _ in 0..10_000_000 {
        i += 1;
    }
    i
}

#[no_mangle]
pub extern "C" fn print_types() {}
