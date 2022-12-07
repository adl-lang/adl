use gumdrop::Options;
use std::path::PathBuf;

// Define options for the program.
#[derive(Debug, Options)]
pub struct CliOptions {
    #[options(help = "print help message")]
    pub help: bool,
    #[options(command)]
    pub command: Option<Command>,
}

#[derive(Debug, Options)]
pub enum Command {
    #[options(help = "verify ADL")]
    Verify(VerifyOpts),
}

#[derive(Debug, Options)]
pub struct VerifyOpts {
    #[options(help = "adds the given directory to the ADL search path", meta = "I")]
    pub searchdir: Vec<PathBuf>,
    #[options(free)]
    pub modules: Vec<String>,
}
