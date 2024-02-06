/* main.rs */

// sl         # launch REPL
// sl xxx.sl  # run as script

use std::path::PathBuf;
use std::process::exit;

use clap::Parser;

use cli::execute::execute;
use cli::repl::repl;

mod cli;
mod compiler;
mod frontend;
// mod parser;
mod vm;

#[derive(Parser)]
#[command(
    version,
    about,
    after_help = "If no arguments are provided, it launches a REPL."
)]
struct Args {
    #[arg(help = "Execute <FILE>")]
    file: Option<PathBuf>,
}

#[derive(Debug)]
enum Action {
    Repl,
    Execute(Option<PathBuf>),
}

fn main() {
    let args: Args = Args::parse();

    let action = if args.file.is_some() {
        Action::Execute(args.file)
    } else {
        Action::Repl
    };

    let result = match action {
        Action::Repl => repl(),
        Action::Execute(file) => execute(file),
    };

    if let Err(err) = result {
        eprintln!("{}", err);
        exit(1);
    }
}
