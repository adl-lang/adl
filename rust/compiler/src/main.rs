use compiler::cli::run_cli;

fn main() {
    let exit_code = run_cli();
    std::process::exit(exit_code);
}
