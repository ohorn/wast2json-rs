use clap::Parser;
use std::{collections::HashMap, env, fs, path::PathBuf};
use wast2json::{wast2json, Wast2JsonOptions};

#[derive(Parser, Debug)]
#[command(about, version)]
struct Args {
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    #[arg(
        short = 'p',
        long = "pretty-print",
        group = "json",
        help = "Pretty-print JSON output"
    )]
    pretty_print: bool,

    #[arg(
        short = 'c',
        long = "compact",
        group = "json",
        help = "Compact JSON output"
    )]
    compact: bool,

    #[arg(short, long, value_parser, help = "Output .json file")]
    output: Option<PathBuf>,

    #[arg(value_parser, help = "Input .wast file")]
    input: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    env_logger::builder()
        .filter_level(args.verbose.log_level_filter())
        .parse_default_env()
        .init();

    log::debug!("Arguments: {:?}", args);

    let input = &args.input;
    log::debug!("Input file: {:?}", input);

    let output = match args.output {
        Some(output) => output,
        None => env::current_dir()?
            .join(input.file_name().unwrap_or_default())
            .with_extension("json"),
    };
    log::debug!("Output file: {:?}", output);

    let module_basename = output.file_stem().unwrap_or_default().to_string_lossy();

    let source = fs::read_to_string(input)?;
    let options = Wast2JsonOptions::default();
    let mut module_files = HashMap::new();

    let script = wast2json(
        input,
        &source,
        &module_basename,
        &mut module_files,
        &options,
    )?;

    log::debug!("Writing {:?}", output);
    let file = std::fs::File::create(&output)?;
    if args.compact {
        script.write_compact(file)?;
    } else if args.pretty_print {
        script.write_pretty(file)?;
    } else {
        script.write(file)?;
    }

    for (filename, bytes) in module_files.iter() {
        let filename = output.with_file_name(filename);
        fs::write(filename, bytes)?;
    }

    Ok(())
}
