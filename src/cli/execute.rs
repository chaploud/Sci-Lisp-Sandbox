/* cli/execute.rs */

use std::path::PathBuf;

pub fn execute(file: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    println!("execute: {:?}", file);
    println!("Not implemented yet");
    Ok(())
}
