use compiler::cli::CliOptions;
use gumdrop::Options;

fn main() {
    let opts = CliOptions::parse_args_default_or_exit();

    match opts.command {
        None => println!("{}", CliOptions::usage()),
        Some(cmd) => println!("{:#?}", cmd),
    }
}
