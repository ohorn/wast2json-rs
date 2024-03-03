use assert_json_diff::assert_json_eq;
use std::{collections::HashMap, str::FromStr};
use wast::lexer::{Lexer, TokenKind};
use wast2json::{wast2json, Wast2JsonOptions};

macro_rules! test_wast {
    ( $( $testfile:literal as $name:ident ,)* ) => {
        $(
            #[test]
            fn $name() {
                run_wast_test($testfile);
            }
        )*
    };
}

test_wast! {
    "tests/local/parse/assert/assert-after-module.txt" as assert_after_module,
    "tests/local/parse/assert/assertreturn-arithmetic-nan.txt" as assertreturn_arithmetic_nan,
    "tests/local/parse/assert/assertreturn-canonical-nan.txt" as assertreturn_canonical_nan,
    "tests/local/parse/assert/assertinvalid-binary-module.txt" as assertinvalid_binary_module,
    "tests/local/parse/assert/assertinvalid.txt" as assertinvalid,
    "tests/local/parse/assert/assertmalformed.txt" as assertmalformed,
    "tests/local/parse/assert/assertreturn.txt" as assertreturn,
    "tests/local/parse/assert/bad-assert-before-module.txt" as bad_assert_before_module,
    "tests/local/parse/assert/bad-assertreturn-non-const.txt" as bad_assertreturn_non_const,
    "tests/local/parse/assert/bad-assertreturn-too-few.txt" as bad_assertreturn_too_few,
    "tests/local/parse/assert/bad-assertreturn-too-many.txt" as bad_assertreturn_too_many,
    "tests/local/parse/assert/bad-assertreturn-unknown-function.txt" as bad_assertreturn_unknown_function,
    "tests/local/parse/assert/bad-invoke-no-module.txt" as bad_invoke_no_module,
    "tests/local/parse/assert/bad-invoke-too-few.txt" as bad_invoke_too_few,
    "tests/local/parse/assert/bad-invoke-too-many.txt" as bad_invoke_too_many,
    "tests/local/parse/assert/bad-invoke-unknown-function.txt" as bad_invoke_unknown_function,
    "tests/local/parse/assert/invoke.txt" as invoke,
    "tests/local/parse/bad-input-command.txt" as bad_input_command,
    "tests/local/parse/bad-output-command.txt" as bad_output_command,
    "tests/local/typecheck/bad-assertreturn-invoke-type-mismatch.txt" as bad_assertreturn_invoke_type_mismatch,
    "tests/local/typecheck/bad-assertreturn-type-mismatch.txt" as bad_assertreturn_type_mismatch,
    "tests/local/typecheck/bad-invoke-type-mismatch.txt" as bad_invoke_type_mismatch,
    "tests/local/module-binary.txt" as module_binary,
}

#[derive(Debug)]
struct TestCommand {
    key: String,
    arg: Option<String>,
    payload: String,
}
fn run_wast_test(testfile: &str) {
    let filename = std::path::PathBuf::from_str(testfile).expect("path");
    let content = std::fs::read_to_string(&filename).unwrap();
    let options = Wast2JsonOptions::default();
    let mut cmds = parse_test_commands(&content);
    let mut module_files = HashMap::new();
    let mut result = None;
    for cmd in cmds.iter_mut() {
        match cmd.key.as_str() {
            "TOOL" => {
                assert_eq!(Some("wast2json"), cmd.arg.as_deref(), "tool");
                result = Some(wast2json(
                    &filename,
                    &content,
                    &filename.file_stem().unwrap_or_default().to_string_lossy(),
                    &mut module_files,
                    &options,
                ));
            }
            "ERROR" => {
                assert_eq!(Some("1"), cmd.arg.as_deref());
                assert!(result.as_ref().filter(|r| r.is_err()).is_some(), "error");
            }
            "STDERR" => {
                let result = result.as_ref().expect("result");
                assert!(result.is_err(), "error expected: {}", cmd.payload);
                assert_eq!(
                    cmd.payload.trim_end(),
                    format!("{}", result.as_ref().err().unwrap()),
                    "error message"
                );
            }
            "OUTPUT" => {
                let result = result.as_ref().expect("result");
                if let Err(ref e) = result {
                    panic!("unexpected error: {}", e);
                }
                eprintln!("{}", &cmd.payload);
                let json = serde_json::from_str::<serde_json::Value>(&cmd.payload).unwrap();
                assert_json_eq!(json, result.as_ref().unwrap());
            }
            "IGNORE" | "BINARY" | "DUMP" => {
                let arg = cmd.arg.as_ref().unwrap();
                let module = module_files.remove(arg).expect(arg);
                if cmd.key == "DUMP" {
                    let dump = wasmprinter::print_bytes(module).unwrap();
                    assert_eq!(&dump, &cmd.payload, "dump {}", arg);
                } else if cmd.key == "BINARY" {
                    let lexer = Lexer::new(&cmd.payload);
                    let token = lexer.iter(0).next().expect("first token").expect("token");
                    if token.kind != TokenKind::String {
                        panic!("Byte string expected");
                    };
                    let bytes = token.string(&cmd.payload);
                    assert_eq!(bytes, module, "binary {}", arg);
                }
            }
            _ => panic!("{}: Unsupported cmd '{}'", testfile, cmd.key),
        }
    }

    if result.expect("result").is_ok() {
        assert!(module_files.is_empty());
    }
}

fn parse_test_commands(content: &str) -> Vec<TestCommand> {
    let mut cmds = Vec::new();

    for token in Lexer::new(content).iter(0) {
        let token = token.expect("token");
        let cmd = match token.kind {
            TokenKind::LineComment => token.src(content).strip_prefix(";;;"),
            TokenKind::BlockComment => token.src(content).strip_prefix("(;;"),
            _ => None,
        };
        if let Some(cmd) = cmd {
            let mut lines = cmd.lines();
            let first_line = lines.next().unwrap();
            let mut payload: &str = &lines.collect::<Vec<_>>().join("\n");

            let first_line = first_line.trim_end_matches(";;;").trim();
            let (key, arg) = if let Some((k, a)) = first_line.split_once([':', ' ']) {
                (k.trim_end().to_string(), Some(a.trim_start().to_string()))
            } else {
                (first_line.to_string(), None)
            };

            if payload.ends_with(";;)") {
                let suffix = format!(";;; {} ;;)", first_line);
                payload = payload.strip_suffix(&suffix).expect(&suffix);
            }
            cmds.push(TestCommand {
                key,
                arg,
                payload: payload.to_string(),
            });
        }
    }

    cmds
}
