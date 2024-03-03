use std::{io::Write, iter::once, path::PathBuf};

use serde::Serialize;
use serde_json::ser::{CompactFormatter, PrettyFormatter};

mod error;
pub use error::Error;
use error::{ErrorKind, Result, TypeContext};

pub mod json;
mod registry;

use json::{
    Clone2, ModuleType, RefIndex, WabtFormatter, WastJsonAction, WastJsonCommand, WastJsonConst,
    WastJsonExpected, WastJsonScript, WastJsonValueType,
};
use registry::ModuleRegistry;

use wasmparser::ValType;
use wast::{
    core::{HeapType, WastArgCore, WastRetCore},
    parser::{self, ParseBuffer},
    token::{Id, Span},
    QuoteWat, Wast, WastArg, WastDirective, WastExecute, WastInvoke, WastRet, Wat,
};

#[derive(Default)]
pub struct Wast2JsonOptions {}

impl WastJsonScript {
    pub fn to_string() {}

    pub fn write<W: Write>(&self, writer: W) -> Result<()> {
        self.write_formatted(writer, WabtFormatter::new())
    }

    pub fn write_pretty<W: Write>(&self, writer: W) -> Result<()> {
        self.write_formatted(writer, PrettyFormatter::new())
    }

    pub fn write_compact<W: Write>(&self, writer: W) -> Result<()> {
        self.write_formatted(writer, CompactFormatter)
    }

    pub fn write_formatted<W, F>(&self, writer: W, formatter: F) -> Result<()>
    where
        W: Write,
        F: serde_json::ser::Formatter + std::fmt::Debug,
    {
        let mut serializer = serde_json::ser::Serializer::with_formatter(writer, formatter);
        self.serialize(&mut serializer)?;
        Ok(())
    }
}

/// Trait for writing (or collecting) the `.wat` or `.wasm` modules files
/// associated with a [`WastJsonScript`].
pub trait WriteModuleFile {
    fn write(&mut self, module_fileme: &str, bytes: Vec<u8>) -> Result<()>;
}

impl<T> WriteModuleFile for T
where
    T: Extend<(String, Vec<u8>)>,
{
    fn write(&mut self, module_filename: &str, bytes: Vec<u8>) -> Result<()> {
        self.extend(Some((module_filename.to_string(), bytes)));
        Ok(())
    }
}

/// Converts a `wast` file to its `wast2json` representation.
pub fn wast2json<M: WriteModuleFile>(
    source_filename: &PathBuf,
    source: &str,
    module_basename: &str,
    module_files: &mut M,
    options: &Wast2JsonOptions,
) -> Result<WastJsonScript> {
    let wast2json = Wast2JsonConverter::new(
        source_filename,
        source,
        module_basename,
        module_files,
        options,
    );
    wast2json.convert()
}

struct Wast2JsonConverter<'a, M: WriteModuleFile> {
    filename: &'a PathBuf,
    text: &'a str,
    line_offsets: Vec<usize>,
    module_basename: &'a str,
    module_files: &'a mut M,
    counter: u32,
    registry: ModuleRegistry,
}

#[derive(Clone)]
struct ModuleFile {
    pub filename: String,
    pub module_type: ModuleType,
    pub name: Option<String>,
}

macro_rules! unsupported {
    ($str:expr) => {
        return Err(Error::new(ErrorKind::UnsupportedFeature($str)))
    };
}

impl<'a, M: WriteModuleFile> Wast2JsonConverter<'a, M> {
    fn new(
        source_filename: &'a PathBuf,
        text: &'a str,
        module_basename: &'a str,
        module_files: &'a mut M,
        _options: &Wast2JsonOptions,
    ) -> Self {
        let line_offsets = once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        Self {
            filename: source_filename,
            text,
            line_offsets,
            module_basename,
            module_files,
            counter: 0,
            registry: ModuleRegistry::new(),
        }
    }

    fn span_linecol(&self, span: Span) -> (usize, usize) {
        let offset = span.offset();
        let line = self.line_offsets.partition_point(|&i| i <= offset) - 1;
        let line_offset = self.line_offsets.get(line).copied().unwrap_or(0);
        (line, offset - line_offset)
    }

    fn convert(mut self) -> Result<WastJsonScript> {
        let buf = ParseBuffer::new(self.text)?;
        let mut wast = match parser::parse::<Wast>(&buf) {
            Ok(wast) => wast,
            Err(err) => {
                let mut err: Error = err.into();
                err.set_source(self.filename, self.text);
                return Err(err);
            }
        };

        let mut cmds = Vec::new();

        for directive in &mut wast.directives {
            let cmd = self.directive_to_command(directive).map_err(|mut err| {
                err.set_span(directive.span());
                err.set_source(self.filename, self.text);
                err
            })?;
            cmds.push(cmd);
        }

        Ok(WastJsonScript {
            source_filename: self.filename.to_string_lossy().to_string(),
            commands: cmds.into_boxed_slice(),
        })
    }

    #[allow(clippy::too_many_lines)]
    fn directive_to_command(&mut self, directive: &mut WastDirective) -> Result<WastJsonCommand> {
        let span = directive.span();
        let line = self.span_linecol(span).0 + 1;

        let cmd = match directive {
            WastDirective::Wat(wat) => {
                let file = self.write_quote_module(wat)?;
                let name = file.name;
                let filename = file.filename;
                WastJsonCommand::Module {
                    line,
                    name,
                    filename,
                }
            }

            WastDirective::Invoke(invoke) => {
                let result_types = self.check_types_invoke(invoke)?;
                WastJsonCommand::Action {
                    line,
                    action: self.invoke_to_action(invoke)?,
                    expected: result_types,
                }
            }

            WastDirective::AssertReturn { exec, results, .. } => {
                self.check_result_types(exec, results)?;
                let expected = results
                    .iter()
                    .map(|v| v.try_into())
                    .collect::<Result<Box<[_]>>>()?;
                WastJsonCommand::AssertReturn {
                    line,
                    action: self.exec_to_action(exec)?,
                    expected,
                }
            }

            WastDirective::AssertException { ref exec, .. } => {
                let result_types = self.check_types(exec)?;
                WastJsonCommand::AssertException {
                    line,
                    action: self.exec_to_action(exec)?,
                    expected: result_types,
                }
            }

            WastDirective::AssertMalformed {
                module, message, ..
            } => {
                let file = self.write_quote_module(module)?;
                WastJsonCommand::AssertMalformed {
                    line,
                    filename: file.filename.to_string(),
                    text: (*message).to_string(),
                    module_type: file.module_type,
                }
            }

            WastDirective::AssertInvalid {
                module, message, ..
            } => {
                let file = self.write_quote_module(module)?;
                WastJsonCommand::AssertInvalid {
                    line,
                    filename: file.filename.to_string(),
                    text: (*message).to_string(),
                    module_type: file.module_type,
                }
            }

            WastDirective::AssertTrap {
                exec: WastExecute::Wat(wat),
                message,
                ..
            } => {
                let file = self.write_module(wat)?;
                WastJsonCommand::AssertUninstantiable {
                    line,
                    filename: file.filename.to_string(),
                    text: (*message).to_string(),
                    module_type: file.module_type,
                }
            }

            WastDirective::AssertUnlinkable {
                module, message, ..
            } => {
                let file = self.write_module(module)?;
                WastJsonCommand::AssertUnlinkable {
                    line,
                    filename: file.filename.to_string(),
                    text: (*message).to_string(),
                    module_type: file.module_type,
                }
            }

            WastDirective::AssertTrap { exec, message, .. } => {
                let result_types = self.check_types(exec)?;
                WastJsonCommand::AssertTrap {
                    line,
                    action: self.exec_to_action(exec)?,
                    text: (*message).to_string(),
                    expected: result_types,
                }
            }

            WastDirective::AssertExhaustion { call, message, .. } => {
                let result_types = self.check_types_invoke(call)?;
                WastJsonCommand::AssertExhaustion {
                    line,
                    action: self.invoke_to_action(call)?,
                    text: (*message).to_string(),
                    expected: result_types,
                }
            }

            WastDirective::Register {
                name: alias,
                module,
                ..
            } => {
                // Check that the module exists
                self.registry.lookup_module(module)?;
                WastJsonCommand::Register {
                    line,
                    name: module.map(|ref id| id_to_name(id)),
                    alias: (*alias).to_string(),
                }
            }

            WastDirective::Thread(..) | WastDirective::Wait { .. } => unsupported!("threads"),
        };

        Ok(cmd)
    }

    fn invoke_to_action(&self, invoke: &WastInvoke) -> Result<WastJsonAction> {
        let module = invoke.module.map(|ref id| id_to_name(id));
        let field = invoke.name.to_string();
        let args = invoke
            .args
            .iter()
            .map(|v| v.try_into())
            .collect::<Result<Box<[_]>>>()?;
        Ok(WastJsonAction::Invoke {
            module,
            field,
            args,
        })
    }

    fn exec_to_action(&self, exec: &WastExecute) -> Result<WastJsonAction> {
        match exec {
            WastExecute::Invoke(invoke) => self.invoke_to_action(invoke),
            WastExecute::Get { global, module } => {
                let module = module.map(|ref id| id_to_name(id));
                let field = (*global).to_string();
                Ok(WastJsonAction::Get { module, field })
            }
            WastExecute::Wat(..) => unsupported!("components"),
        }
    }

    fn write_quote_module(&mut self, module: &mut QuoteWat) -> Result<ModuleFile> {
        match module {
            QuoteWat::QuoteModule(_, source) => self.write_quote(source),
            QuoteWat::QuoteComponent(..) => unsupported!("components"),
            QuoteWat::Wat(wat) => self.write_module(wat),
        }
    }

    fn write_module(&mut self, module: &mut Wat) -> Result<ModuleFile> {
        match module {
            Wat::Module(module) => {
                let bytes = module.encode()?;
                let name = module.id.map(|ref id| id_to_name(id));
                self.registry.define(&module.id, &bytes)?;
                self.write(ModuleType::Binary, name, bytes)
            }
            Wat::Component(..) => unsupported!("components"),
        }
    }

    fn write_quote(&mut self, source: &[(Span, &[u8])]) -> Result<ModuleFile> {
        let len = source.iter().map(|&(_, buf)| buf.len()).sum();
        let mut bytes = Vec::with_capacity(len);
        source.iter().for_each(|&(_, buf)| bytes.extend(buf));
        self.write(ModuleType::Text, None, bytes)
    }

    fn write(
        &mut self,
        module_type: ModuleType,
        name: Option<String>,
        bytes: Vec<u8>,
    ) -> Result<ModuleFile> {
        let filename = format!(
            "{}.{}.{}",
            self.module_basename,
            self.counter,
            module_type.extension()
        );

        self.counter += 1;

        let file = ModuleFile {
            filename: filename.clone(),
            module_type,
            name,
        };

        self.module_files.write(&filename, bytes)?;

        Ok(file)
    }

    fn check_types(&self, exec: &WastExecute) -> Result<Box<[WastJsonValueType]>> {
        match exec {
            WastExecute::Invoke(invoke) => self.check_types_invoke(invoke),
            WastExecute::Get { module, global } => {
                let module = self.registry.lookup_module(module)?;
                let global_type = module.get_global_type(global)?;
                Ok(Box::new([global_type.try_into()?]))
            }
            WastExecute::Wat(..) => unsupported!("components"),
        }
    }

    fn check_types_invoke(&self, invoke: &WastInvoke) -> Result<Box<[WastJsonValueType]>> {
        let result = || -> Result<_> {
            let module = self.registry.lookup_module(&invoke.module)?;
            let func_type = module.get_func_type(invoke.name)?;
            let params = func_type
                .params()
                .iter()
                .map(|t| t.try_into())
                .collect::<Result<Vec<_>>>()?;
            self.check_type_slices("invoke", TypeContext::Argument, &params, &invoke.args)?;
            func_type.results().iter().map(|t| t.try_into()).collect()
        };
        result().map_err(|mut e| {
            e.set_span(invoke.span);
            e
        })
    }

    fn check_result_types(&self, exec: &WastExecute, assert_exprs: &[wast::WastRet]) -> Result<()> {
        let result_types = self.check_types(exec)?;
        self.check_type_slices("action", TypeContext::Result, &result_types, assert_exprs)
    }

    fn check_type_slices<T: GetType>(
        &self,
        desc: &str,
        context: TypeContext,
        expected: &[WastJsonValueType],
        exprs: &[T],
    ) -> Result<()> {
        if exprs.len() != expected.len() {
            return Err(Error::new(ErrorKind::WrongNumberOfTypes(
                context,
                exprs.len(),
                expected.len(),
            )));
        }
        for (index, (expr, &expected_type)) in exprs.iter().zip(expected.iter()).enumerate() {
            let expr_type = expr.get_type()?;
            if expr_type != expected_type {
                return Err(Error::new(ErrorKind::WrongType(
                    desc.to_string(),
                    context,
                    index,
                    expr_type,
                    expected_type,
                )));
            }
        }
        Ok(())
    }
}

fn id_to_name(id: &Id) -> String {
    format!("${}", id.name())
}

trait GetType {
    fn get_type(&self) -> Result<WastJsonValueType>;
}

impl GetType for WastRetCore<'_> {
    fn get_type(&self) -> Result<WastJsonValueType> {
        Ok(match self {
            WastRetCore::I32(_) => WastJsonValueType::I32,
            WastRetCore::I64(_) => WastJsonValueType::I64,
            WastRetCore::F32(_) => WastJsonValueType::F32,
            WastRetCore::F64(_) => WastJsonValueType::F64,
            WastRetCore::V128(_) => WastJsonValueType::V128,
            WastRetCore::RefFunc(_) => WastJsonValueType::FuncRef,
            WastRetCore::RefNull(Some(HeapType::Func)) => WastJsonValueType::FuncRef,
            WastRetCore::RefExtern(_) => WastJsonValueType::ExternRef,
            WastRetCore::RefNull(Some(HeapType::Extern)) => WastJsonValueType::ExternRef,
            WastRetCore::Either(_) => unsupported!("either"),
            _ => unsupported!("gc"),
        })
    }
}

impl GetType for WastRet<'_> {
    fn get_type(&self) -> Result<WastJsonValueType> {
        match self {
            WastRet::Core(core) => core.get_type(),
            WastRet::Component(_) => unsupported!("components"),
        }
    }
}

impl GetType for WastArgCore<'_> {
    fn get_type(&self) -> Result<WastJsonValueType> {
        Ok(match self {
            WastArgCore::I32(_) => WastJsonValueType::I32,
            WastArgCore::I64(_) => WastJsonValueType::I64,
            WastArgCore::F32(_) => WastJsonValueType::F32,
            WastArgCore::F64(_) => WastJsonValueType::F64,
            WastArgCore::V128(_) => WastJsonValueType::V128,
            WastArgCore::RefNull(HeapType::Func) => WastJsonValueType::FuncRef,
            WastArgCore::RefExtern(_) => WastJsonValueType::ExternRef,
            WastArgCore::RefNull(HeapType::Extern) => WastJsonValueType::ExternRef,
            _ => unsupported!("gc"),
        })
    }
}

impl GetType for WastArg<'_> {
    fn get_type(&self) -> Result<WastJsonValueType> {
        match self {
            WastArg::Core(core) => core.get_type(),
            WastArg::Component(_) => unsupported!("components"),
        }
    }
}

impl TryFrom<&ValType> for WastJsonValueType {
    type Error = Error;

    fn try_from(valtype: &ValType) -> Result<Self> {
        Ok(match valtype {
            ValType::I32 => WastJsonValueType::I32,
            ValType::I64 => WastJsonValueType::I64,
            ValType::F32 => WastJsonValueType::F32,
            ValType::F64 => WastJsonValueType::F64,
            ValType::V128 => WastJsonValueType::V128,
            ValType::Ref(r) if r.is_func_ref() => WastJsonValueType::FuncRef,
            ValType::Ref(r) if r.is_extern_ref() => WastJsonValueType::ExternRef,
            ValType::Ref(_) => unsupported!("reference types"),
        })
    }
}

impl<'a> TryFrom<&WastRet<'a>> for WastJsonExpected {
    type Error = Error;

    fn try_from(value: &WastRet<'a>) -> Result<Self> {
        match value {
            WastRet::Core(ret) => ret.try_into(),
            WastRet::Component(_) => unsupported!("components"),
        }
    }
}

impl<'a> TryFrom<&WastArg<'a>> for WastJsonConst {
    type Error = Error;

    fn try_from(value: &WastArg<'a>) -> Result<Self> {
        match value {
            WastArg::Core(arg) => arg.try_into(),
            WastArg::Component(_) => unsupported!("components"),
        }
    }
}

impl<'a> TryFrom<&WastRetCore<'a>> for WastJsonExpected {
    type Error = Error;

    fn try_from(value: &WastRetCore<'a>) -> Result<Self> {
        Ok(match value {
            WastRetCore::I32(val) => WastJsonExpected::I32 { value: *val },
            WastRetCore::I64(val) => WastJsonExpected::I64 { value: *val },
            WastRetCore::F32(val) => WastJsonExpected::F32 {
                value: val.clone2(),
            },
            WastRetCore::F64(val) => WastJsonExpected::F64 {
                value: val.clone2(),
            },
            WastRetCore::V128(val) => WastJsonExpected::V128(val.into()),
            WastRetCore::RefNull(Some(HeapType::Func)) => WastJsonExpected::FuncRef {
                value: RefIndex::Null,
            },
            WastRetCore::RefNull(Some(HeapType::Extern)) => WastJsonExpected::ExternRef {
                value: RefIndex::Null,
            },
            WastRetCore::RefExtern(Some(val)) => WastJsonExpected::ExternRef {
                value: RefIndex::Index(*val),
            },
            WastRetCore::Either(..) => unsupported!("either"),
            _ => unsupported!("gc"),
        })
    }
}

impl<'a> TryFrom<&WastArgCore<'a>> for WastJsonConst {
    type Error = Error;

    fn try_from(value: &WastArgCore<'a>) -> Result<Self> {
        Ok(match value {
            WastArgCore::I32(val) => WastJsonConst::I32 { value: *val },
            WastArgCore::I64(val) => WastJsonConst::I64 { value: *val },
            WastArgCore::F32(val) => WastJsonConst::F32 { value: *val },
            WastArgCore::F64(val) => WastJsonConst::F64 { value: *val },
            WastArgCore::V128(val) => WastJsonConst::V128(val.into()),
            WastArgCore::RefNull(HeapType::Func) => WastJsonConst::FuncRef {
                value: RefIndex::Null,
            },
            WastArgCore::RefNull(HeapType::Extern) => WastJsonConst::ExternRef {
                value: RefIndex::Null,
            },
            WastArgCore::RefExtern(val) => WastJsonConst::ExternRef {
                value: RefIndex::Index(*val),
            },
            _ => unsupported!("gc"),
        })
    }
}
