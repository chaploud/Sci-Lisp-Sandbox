/* cli/repl.rs */

use std::borrow::Cow;
use std::env;

use colored::*;
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::validate::MatchingBracketValidator;
use rustyline::{CompletionType, Config, EditMode, Editor};
use rustyline_derive::{Completer, Helper, Hinter, Validator};

const HISTORY_FILE: &str = "./.scilisp-history.txt";

#[derive(Helper, Hinter, Validator, Completer)]
struct RLHelper {
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,

    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

impl Highlighter for RLHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        let highlighted = self.highlighter.highlight(line, pos);

        // HACK:
        let colored = highlighted.replace("\x1b[1;34m", "\x1b[35;47m");

        Cow::Owned(colored)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        self.highlighter.highlight_char(line, pos, forced)
    }
}

fn say_goodbye() {
    eprintln!("{}", "[Bye!]".purple());
}

pub fn repl() -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("Sci-Lisp v{}", env!("CARGO_PKG_VERSION"));

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();

    let helper = RLHelper {
        highlighter: MatchingBracketHighlighter::new(),
        validator: MatchingBracketValidator::new(),
    };

    let mut rl = Editor::with_config(config).unwrap();
    rl.set_helper(Some(helper));

    if rl.load_history(HISTORY_FILE).is_err() {
        eprintln!("No previous history.");
    }

    loop {
        let readline = rl.readline("λ > ".bold().purple().to_string().as_str());
        match readline {
            Err(ReadlineError::Interrupted) => {
                eprintln!("Ctrl-C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                eprintln!("Ctrl-D");
                say_goodbye();
                break;
            }
            Err(err) => {
                eprintln!("{}", err);
                continue;
            }
            Ok(line) => {
                rl.add_history_entry(&line)?;

                if line == "exit" {
                    say_goodbye();
                    break;
                }

                if line.trim().is_empty() {
                    continue;
                }
            }
        };
    }

    rl.save_history(HISTORY_FILE)?;
    Ok(())
}
