use crate::{
    error::{Error, ErrorKind, Result},
    Wast2JsonOptions,
};
use std::{collections::HashMap, rc::Rc};
use wasmparser::{ExternalKind, FuncType, Payload, ValType, Validator};
use wast::token::Id;

/// Module registry
pub struct ModuleRegistry<'a> {
    options: &'a Wast2JsonOptions,
    named: HashMap<String, Rc<ModuleExports>>,
    most_recent: Option<Rc<ModuleExports>>,
}

/// A disassembled wasm module
pub struct ModuleExports(HashMap<String, Export>);

#[derive(Clone)]
pub enum Export {
    Global(ValType),
    Func(FuncType),
    Other,
}

impl<'a> ModuleRegistry<'a> {
    pub fn new(options: &'a Wast2JsonOptions) -> Self {
        Self {
            options,
            named: HashMap::new(),
            most_recent: None,
        }
    }

    pub fn define(&mut self, name: &Option<Id>, bytes: &[u8]) -> Result<()> {
        let exports = Rc::new(ModuleExports::parse(bytes, self.options)?);
        if let Some(id) = name {
            self.named.insert(id.name().into(), exports.clone());
        }
        self.most_recent.replace(exports);
        Ok(())
    }

    pub fn lookup_module(&self, name: &Option<Id>) -> Result<&ModuleExports> {
        let module = match name {
            Some(id) => self.named.get(id.name()),
            None => self.most_recent.as_ref(),
        };
        module
            .map(|m| m.as_ref())
            .ok_or_else(|| Error::new(ErrorKind::UnknownModule))
    }
}

impl ModuleExports {
    pub fn parse(bytes: &[u8], options: &Wast2JsonOptions) -> Result<ModuleExports> {
        let mut types: Vec<Export> = Vec::new();
        let mut funcs: Vec<u32> = Vec::new();
        let mut globals: Vec<ValType> = Vec::new();
        let mut exports: HashMap<String, Export> = HashMap::new();
        let mut validator = options
            .validate
            .then(|| Validator::new_with_features(options.features));

        for item in wasmparser::Parser::new(0).parse_all(bytes) {
            let payload = item?;
            if let Some(ref mut validator) = validator {
                validator.payload(&payload)?;
            }
            match payload {
                Payload::TypeSection(s) => {
                    types.reserve(s.count() as usize);
                    for ty in s.into_iter_err_on_gc_types() {
                        types.push(ty.map_or(Export::Other, Export::Func));
                    }
                }
                Payload::ImportSection(s) => {
                    for import in s {
                        let ty = import?.ty;
                        if let wasmparser::TypeRef::Func(f) = ty {
                            funcs.push(f);
                        } else if let wasmparser::TypeRef::Global(g) = ty {
                            globals.push(g.content_type);
                        }
                    }
                }
                Payload::FunctionSection(s) => {
                    types.reserve_exact(s.count() as usize);
                    for f in s {
                        funcs.push(f?);
                    }
                }
                Payload::GlobalSection(s) => {
                    for g in s {
                        globals.push(g?.ty.content_type);
                    }
                }
                Payload::ExportSection(s) => {
                    for export in s {
                        let export = export?;
                        let x = match export.kind {
                            ExternalKind::Func => funcs
                                .get(export.index as usize)
                                .and_then(|&i| types.get(i as usize))
                                .cloned()
                                .unwrap_or(Export::Other),
                            ExternalKind::Global => globals
                                .get(export.index as usize)
                                .map_or(Export::Other, |&t| Export::Global(t)),
                            _ => Export::Other,
                        };
                        exports.insert(export.name.to_string(), x);
                    }
                }
                _ => (),
            }
        }

        Ok(ModuleExports(exports))
    }

    pub fn get_func_type(&self, name: &str) -> Result<&FuncType> {
        if let Some(Export::Func(ref t)) = self.0.get(name) {
            Ok(t)
        } else {
            Err(Error::new(ErrorKind::UnknownFuncExport(name.into())))
        }
    }

    pub fn get_global_type(&self, name: &str) -> Result<&ValType> {
        if let Some(Export::Global(ref t)) = self.0.get(name) {
            Ok(t)
        } else {
            Err(Error::new(ErrorKind::UnknownGlobalExport(name.into())))
        }
    }
}
