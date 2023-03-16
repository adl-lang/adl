use clap::{Args, Parser};
use std::path::PathBuf;

pub mod ast;
pub mod rust;
pub mod tsgen;
pub mod verify;





pub fn run_cli() -> i32 {
    let cli = Cli::parse();

    let r = match cli.command {
        Command::Verify(opts) => verify::verify(&opts),
        Command::Ast(opts) => ast::ast(&opts),
        Command::Rust(opts) => rust::rust(&opts),
        Command::Tsgen(opts) => tsgen::tsgen(&opts),
    };
    match r {
        Ok(_) => 0,
        Err(err) => {
            log::error!("{}", err);
            1
        }
    }
}

#[derive(Parser)]
#[command(name = "adlc")]
#[command(author = "Tim Docker")]
#[command(version = "0.1")]
#[command(about = "ADL code generation cli tool", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Parser)]
pub enum Command {
    /// verify ADL
    Verify(VerifyOpts),
    /// generate the json AST for some ADL modules
    Ast(AstOpts),
    /// generate rust code for the some ADL modules
    Rust(RustOpts),
    /// generate typescript code for the some ADL modules
    Tsgen(TsOpts),
}

#[derive(Debug, Args)]
pub struct VerifyOpts {
    #[clap(flatten)]
    pub search: AdlSearchOpts,

    pub modules: Vec<String>,
}

#[derive(Debug, Args)]
pub struct AstOpts {
    #[clap(flatten)]
    pub search: AdlSearchOpts,

    /// writes the AST to the specified file"
    #[arg(long, short='O', value_name="FILE")]
    pub outfile: Option<PathBuf>,

    pub modules: Vec<String>,
}

#[derive(Debug, Args)]
pub struct RustOpts {
    #[clap(flatten)]
    pub search: AdlSearchOpts,

    #[clap(flatten)]
    pub output: OutputOpts,

    /// Generate the runtime code
    #[arg(long)]
    pub include_runtime: bool,

    /// The module where the code is generated, relative to crate root
    #[arg(long, value_name="RSMODULE", default_value_t={"adl".to_string()})]
    pub module: String,

    /// The module where the runtime is located, relative to crate root
    #[arg(long, value_name="RSMODULE", default_value_t={"adlrt".to_string()})]
    pub runtime_module: String,

    #[arg(value_name="ADLMODULE")]
    pub modules: Vec<String>,
}


#[derive(Debug, Args)]
pub struct TsOpts {
    #[clap(flatten)]
    pub search: AdlSearchOpts,

    // #[clap(flatten)]
    // pub output: OutputOpts,

    /// Generate the runtime code
    #[arg(long)]
    pub include_runtime: bool,

    #[arg(value_name="ADLMODULE")]
    pub modules: Vec<String>,
}

#[derive(Debug, Args)]
pub struct AdlSearchOpts {
    /// adds the given directory to the ADL search path
    #[arg(long="searchdir", short='I', value_name="DIR")]
    pub path: Vec<PathBuf>,
}

#[derive(Debug, Args)]
pub struct OutputOpts {
    /// writes generated code to the specified directory
    #[arg(long, short='O', value_name="DIR")]
    pub outdir: PathBuf,

    /// write a manifest file recording generated files
    #[arg(long, value_name="FILE")]
    pub manifest: Option<PathBuf>,
}

