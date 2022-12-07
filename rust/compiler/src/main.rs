use compiler::cli::{CliOptions, Command};
use gumdrop::Options;

use compiler::cli::verify::verify;

fn main() {
    let opts = CliOptions::parse_args_default_or_exit();

    match opts.command {
        None => println!("{}", CliOptions::self_usage(&opts)),
        Some(Command::Verify(opts)) => verify(&opts),
    }
}
