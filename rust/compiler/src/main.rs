use compiler::cli::run_cli;

fn main() {
    // // useful for recursive stack overflows
    // unsafe { backtrace_on_stack_overflow::enable() };

    env_logger::init();
    let exit_code = run_cli();
    std::process::exit(exit_code);
}
