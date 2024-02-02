use std::error::Error;
use wasmtime::*;

fn main() -> Result<(), Box<dyn Error>> {
    // An engine stores and configures global compilation settings like
    // optimization level, enabled wasm features, etc.
    let engine = Engine::default();

    // We start off by creating a `Module` which represents a compiled form
    // of our input wasm module. In this case it'll be JIT-compiled after
    // we parse the text format.
    let wasm_bytes = include_bytes!("../target/wasm32-wasi/release/core.wasm");
    let module = Module::new(&engine, wasm_bytes)?;
    // let module = Module::new(
    //     &engine,
    //     r#"(module (func (export "answer") (result i32) i32.const 42))"#,
    // )?;

    // A `Store` is what will own instances, functions, globals, etc. All wasm
    // items are stored within a `Store`, and it's what we'll always be using to
    // interact with the wasm world. Custom data can be stored in stores but for
    // now we just use `()`.
    let mut store = Store::new(&engine, ());

    // With a compiled `Module` we can then instantiate it, creating
    // an `Instance` which we can actually poke at functions on.
    let instance = Instance::new(&mut store, &module, &[])?;

    // The `Instance` gives us access to various exported functions and items,
    // which we access here to pull out our `answer` exported function and
    // run it.
    let start = std::time::Instant::now();
    let answer = instance
        .get_func(&mut store, "loop_ten_million_times")
        .expect("`loop_ten_million_times` was not an exported function");

    // There's a few ways we can call the `answer` `Func` value. The easiest
    // is to statically assert its signature with `typed` (in this case
    // asserting it takes no arguments and returns one i32) and then call it.
    let answer = answer.typed::<(), i32>(&store)?;

    // And finally we can call our function! Note that the error propagation
    // with `?` is done to handle the case where the wasm function traps.
    let result = answer.call(&mut store, ())?;
    println!("Answer: {:?}", result);

    let end = std::time::Instant::now();
    println!("Elapsed time: {:?}", end - start);

    Ok(())
}
