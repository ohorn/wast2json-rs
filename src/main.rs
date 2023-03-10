use clap::{Arg, ArgAction, ArgMatches, Error, FromArgMatches, Parser};
use std::{collections::HashMap, env, fs, path::PathBuf};
use wasmparser::WasmFeatures;
use wast2json::{wast2json, Wast2JsonOptions};

#[derive(Parser, Debug)]
#[command(about, version)]
struct Args {
    #[arg(value_parser, help = "Input .wast file")]
    input: PathBuf,

    #[arg(short, long, value_parser, help = "Output .json file")]
    output: Option<PathBuf>,

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

    #[command(flatten)]
    features: FeatureArgs,

    #[arg(long = "no-check", help = "Don't check for invalid modules")]
    no_check: bool,

    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

#[derive(Debug)]
struct FeatureArgs(WasmFeatures);

#[allow(clippy::type_complexity)]
const FEATURE_FLAGS: &[(&str, fn(&mut WasmFeatures) -> &mut bool)] = &[
    ("exceptions", |f| &mut f.exceptions),
    ("mutable-globals", |f| &mut f.mutable_global),
    ("saturating-float-to-int", |f| {
        &mut f.saturating_float_to_int
    }),
    ("sign-extension", |f| &mut f.sign_extension),
    ("simd", |f| &mut f.simd),
    ("threads", |f| &mut f.threads),
    ("function-references", |f| &mut f.function_references),
    ("multi-value", |f| &mut f.multi_value),
    ("tail-call", |f| &mut f.tail_call),
    ("bulk-memory", |f| &mut f.bulk_memory),
    ("reference-types", |f| &mut f.reference_types),
    ("gc", |f| &mut f.gc),
    ("memory64", |f| &mut f.memory64),
    ("multi-memory", |f| &mut f.multi_memory),
    ("extended-const", |f| &mut f.extended_const),
    ("relaxed-simd", |f| &mut f.relaxed_simd),
];

impl clap::Args for FeatureArgs {
    fn augment_args(cmd: clap::Command) -> clap::Command {
        let mut defaults = WasmFeatures::default();
        FEATURE_FLAGS
            .iter()
            .fold(cmd, |cmd, (name, accessor)| {
                cmd.arg(if *accessor(&mut defaults) {
                    Arg::new(name)
                        .long(format!("disable-{name}"))
                        .help(format!("Disable {name} feature"))
                        .action(ArgAction::SetFalse)
                } else {
                    Arg::new(name)
                        .long(format!("enable-{name}"))
                        .help(format!("Enable {name} feature"))
                        .action(ArgAction::SetTrue)
                })
            })
            .arg(
                Arg::new("all")
                    .long("enable-all")
                    .help("Enable all features")
                    .action(ArgAction::SetTrue),
            )
    }

    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        Self::augment_args(cmd)
    }
}

impl FromArgMatches for FeatureArgs {
    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, Error> {
        let mut args = FeatureArgs(WasmFeatures::default());
        args.update_from_arg_matches(matches).map(|_| args)
    }
    fn from_arg_matches_mut(matches: &mut ArgMatches) -> Result<Self, Error> {
        Self::from_arg_matches(matches)
    }

    fn update_from_arg_matches(&mut self, matches: &ArgMatches) -> Result<(), Error> {
        if matches.get_flag("all") {
            self.0 = WasmFeatures::all();
        } else {
            let features = &mut self.0;
            for (name, accessor) in FEATURE_FLAGS {
                *accessor(features) = matches.get_flag(name);
            }
        };
        Ok(())
    }
    fn update_from_arg_matches_mut(&mut self, matches: &mut ArgMatches) -> Result<(), Error> {
        self.update_from_arg_matches(matches)
    }
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

    let options = Wast2JsonOptions {
        features: args.features.0,
        validate: !args.no_check,
    };
    log::debug!("Options: {:?}", &options);

    let module_basename = output.file_stem().unwrap_or_default().to_string_lossy();
    let source = fs::read_to_string(input)?;
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
