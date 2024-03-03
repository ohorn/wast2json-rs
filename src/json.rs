//! Types for a `wast2json`-compatible JSON representation of `wast` scripts.
//!
//! The entry point is the [`WastJsonScript`] struct.
//!
//! Note: This crate reuses the [`Float32`] and [`Float32`] structs and the
//! [`NanPattern`] enum of the [`wast`] crate.
//! Due to some limitations, the `V128Const` and `V128Pattern` enums are
//! _not_ currently reused!

use serde::Serialize;
use serde_json::ser::Formatter;
use std::{borrow::Cow, fmt, io};
use wast::core::NanPattern;
use wast::token::{Float32, Float64};

#[derive(Serialize)]
pub struct WastJsonScript {
    pub source_filename: String,
    pub commands: Box<[WastJsonCommand]>,
}

#[derive(Serialize)]
#[serde(tag = "type", rename_all = "snake_case", deny_unknown_fields)]
pub enum WastJsonCommand {
    Module {
        line: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        filename: String,
    },
    Action {
        line: usize,
        action: WastJsonAction,
        expected: Box<[WastJsonValueType]>,
    },
    AssertReturn {
        line: usize,
        action: WastJsonAction,
        expected: Box<[WastJsonExpected]>,
    },
    AssertException {
        line: usize,
        action: WastJsonAction,
        expected: Box<[WastJsonValueType]>,
    },
    AssertMalformed {
        line: usize,
        filename: String,
        text: String,
        module_type: ModuleType,
    },
    AssertInvalid {
        line: usize,
        filename: String,
        text: String,
        module_type: ModuleType,
    },
    AssertUninstantiable {
        line: usize,
        filename: String,
        text: String,
        module_type: ModuleType,
    },
    AssertUnlinkable {
        line: usize,
        filename: String,
        text: String,
        module_type: ModuleType,
    },
    AssertTrap {
        line: usize,
        action: WastJsonAction,
        text: String,
        expected: Box<[WastJsonValueType]>,
    },
    AssertExhaustion {
        line: usize,
        action: WastJsonAction,
        text: String,
        expected: Box<[WastJsonValueType]>,
    },
    Register {
        line: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        #[serde(rename = "as")]
        alias: String,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ModuleType {
    Binary,
    Text,
}

impl ModuleType {
    pub fn extension(self) -> &'static str {
        match self {
            ModuleType::Binary => "wasm",
            ModuleType::Text => "wat",
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum WastJsonAction {
    Invoke {
        #[serde(skip_serializing_if = "Option::is_none")]
        module: Option<String>,
        field: String,
        args: Box<[WastJsonConst]>,
    },
    Get {
        #[serde(skip_serializing_if = "Option::is_none")]
        module: Option<String>,
        field: String,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum WastJsonValueType {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
    ExternRef,
}

impl fmt::Display for WastJsonValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::V128 => "v128",
            Self::FuncRef => "funcref",
            Self::ExternRef => "externref",
        })
    }
}

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum WastJsonConst {
    I32 {
        #[serde(with = "value_string")]
        value: i32,
    },
    I64 {
        #[serde(with = "value_string")]
        value: i64,
    },
    F32 {
        #[serde(with = "value_string")]
        value: Float32,
    },
    F64 {
        #[serde(with = "value_string")]
        value: Float64,
    },
    V128(V128Const),
    FuncRef {
        #[serde(with = "value_string")]
        value: RefIndex,
    },
    ExternRef {
        #[serde(with = "value_string")]
        value: RefIndex,
    },
}

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "lane_type", content = "value")]
pub enum V128Const {
    #[serde(rename = "i8", with = "value_string_array")]
    I8([i8; 16]),
    #[serde(rename = "i16", with = "value_string_array")]
    I16([i16; 8]),
    #[serde(rename = "i32", with = "value_string_array")]
    I32([i32; 4]),
    #[serde(rename = "i64", with = "value_string_array")]
    I64([i64; 2]),
    #[serde(rename = "f32", with = "value_string_array")]
    F32([Float32; 4]),
    #[serde(rename = "f64", with = "value_string_array")]
    F64([Float64; 2]),
}

impl From<&wast::core::V128Const> for V128Const {
    fn from(value: &wast::core::V128Const) -> V128Const {
        match value {
            wast::core::V128Const::I8x16(v) => Self::I8(*v),
            wast::core::V128Const::I16x8(v) => Self::I16(*v),
            wast::core::V128Const::I32x4(v) => Self::I32(*v),
            wast::core::V128Const::I64x2(v) => Self::I64(*v),
            wast::core::V128Const::F32x4(v) => Self::F32(*v),
            wast::core::V128Const::F64x2(v) => Self::F64(*v),
        }
    }
}

impl From<&V128Const> for wast::core::V128Const {
    fn from(value: &V128Const) -> wast::core::V128Const {
        match value {
            V128Const::I8(v) => Self::I8x16(*v),
            V128Const::I16(v) => Self::I16x8(*v),
            V128Const::I32(v) => Self::I32x4(*v),
            V128Const::I64(v) => Self::I64x2(*v),
            V128Const::F32(v) => Self::F32x4(*v),
            V128Const::F64(v) => Self::F64x2(*v),
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum WastJsonExpected {
    I32 {
        #[serde(with = "value_string")]
        value: i32,
    },
    I64 {
        #[serde(with = "value_string")]
        value: i64,
    },
    F32 {
        #[serde(with = "value_string")]
        value: NanPattern<Float32>,
    },
    F64 {
        #[serde(with = "value_string")]
        value: NanPattern<Float64>,
    },
    V128(V128Pattern),
    FuncRef {
        #[serde(with = "value_string")]
        value: RefIndex,
    },
    ExternRef {
        #[serde(with = "value_string")]
        value: RefIndex,
    },
}

#[derive(Debug, Serialize)]
#[serde(tag = "lane_type", content = "value")]
pub enum V128Pattern {
    #[serde(rename = "i8", with = "value_string_array")]
    I8([i8; 16]),
    #[serde(rename = "i16", with = "value_string_array")]
    I16([i16; 8]),
    #[serde(rename = "i32", with = "value_string_array")]
    I32([i32; 4]),
    #[serde(rename = "i64", with = "value_string_array")]
    I64([i64; 2]),
    #[serde(rename = "f32", with = "value_string_array")]
    F32([NanPattern<Float32>; 4]),
    #[serde(rename = "f64", with = "value_string_array")]
    F64([NanPattern<Float64>; 2]),
}

pub(crate) trait Clone2 {
    fn clone2(&self) -> Self;
}

impl<T: Clone> Clone2 for NanPattern<T> {
    fn clone2(&self) -> Self {
        match self {
            Self::CanonicalNan => Self::CanonicalNan,
            Self::ArithmeticNan => Self::ArithmeticNan,
            Self::Value(v) => Self::Value(v.clone()),
        }
    }
}

impl Clone2 for [NanPattern<Float32>; 4] {
    fn clone2(&self) -> Self {
        [
            self[0].clone2(),
            self[1].clone2(),
            self[2].clone2(),
            self[3].clone2(),
        ]
    }
}

impl Clone2 for [NanPattern<Float64>; 2] {
    fn clone2(&self) -> Self {
        [self[0].clone2(), self[1].clone2()]
    }
}

impl From<&wast::core::V128Pattern> for V128Pattern {
    fn from(value: &wast::core::V128Pattern) -> V128Pattern {
        match value {
            wast::core::V128Pattern::I8x16(v) => Self::I8(*v),
            wast::core::V128Pattern::I16x8(v) => Self::I16(*v),
            wast::core::V128Pattern::I32x4(v) => Self::I32(*v),
            wast::core::V128Pattern::I64x2(v) => Self::I64(*v),
            wast::core::V128Pattern::F32x4(v) => Self::F32(v.clone2()),
            wast::core::V128Pattern::F64x2(v) => Self::F64(v.clone2()),
        }
    }
}

impl From<&V128Pattern> for wast::core::V128Pattern {
    fn from(value: &V128Pattern) -> wast::core::V128Pattern {
        match value {
            V128Pattern::I8(v) => Self::I8x16(*v),
            V128Pattern::I16(v) => Self::I16x8(*v),
            V128Pattern::I32(v) => Self::I32x4(*v),
            V128Pattern::I64(v) => Self::I64x2(*v),
            V128Pattern::F32(v) => Self::F32x4(v.clone2()),
            V128Pattern::F64(v) => Self::F64x2(v.clone2()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum RefIndex {
    Null,
    Index(u32),
}

impl fmt::Display for RefIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Index(index) => index.fmt(f),
        }
    }
}

mod value_string {
    use super::ValueString;

    pub(crate) fn serialize<S, V>(value: &V, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        V: ValueString,
    {
        serializer.serialize_str(&value.to_value_string())
    }
}

mod value_string_array {
    use super::ValueString;
    use serde::ser::SerializeSeq;

    pub(crate) fn serialize<S, V, const N: usize>(
        value: &[V; N],
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        V: ValueString,
    {
        let mut seq = serializer.serialize_seq(Some(value.len()))?;
        for element in value {
            seq.serialize_element(&element.to_value_string())?;
        }
        seq.end()
    }
}

/// Trait for converting values to their `wast2json` string representation.
///
/// Note: Number values are always written as unsigned decimal numbers.
/// Floats are always written as the decimal encoding of their binary
/// representation.
pub trait ValueString: Sized + fmt::Debug {
    const NAME: &'static str;

    fn to_value_string(&self) -> Cow<'static, str>;
}

macro_rules! value_string {
    ( $name:literal, $ty:ident, $val:ident => $to:expr, $str:ident => $parse:expr ) => {
        impl ValueString for $ty {
            const NAME: &'static str = concat!($name, " value");

            fn to_value_string(&self) -> Cow<'static, str> {
                let $val = self;
                ($to).to_string().into()
            }
        }
    };
}

macro_rules! nanpattern_string {
    ( $name:literal, $ty:ident) => {
        impl ValueString for NanPattern<$ty> {
            const NAME: &'static str = concat!($name, " pattern");

            fn to_value_string(&self) -> Cow<'static, str> {
                match self {
                    Self::CanonicalNan => "nan:canonical".into(),
                    Self::ArithmeticNan => "nan:arithmetic".into(),
                    Self::Value(v) => v.to_value_string(),
                }
            }
        }
    };
}

value_string!("i8", i8, val => *val as u8, str => str.parse::<u8>());
value_string!("i16", i16, val => *val as u16, str => str.parse::<u16>());
value_string!("i32", i32, val => *val as u32, str => str.parse::<u32>());
value_string!("i64", i64, val => *val as u64, str => str.parse::<u64>());
value_string!("f32", Float32, val => val.bits, str => str.parse().map(|bits| Float32 { bits }));
value_string!("f64", Float64, val => val.bits, str => str.parse().map(|bits| Float64 { bits }));

nanpattern_string!("f32", Float32);
nanpattern_string!("f64", Float64);

impl ValueString for RefIndex {
    const NAME: &'static str = "reference";

    #[allow(clippy::cast_sign_loss)]
    fn to_value_string(&self) -> Cow<'static, str> {
        match self {
            RefIndex::Null => "null".into(),
            RefIndex::Index(idx) => idx.to_string().into(),
        }
    }
}

/// A formatter that formats the JSON output similar to wabt.
#[derive(Clone, Debug)]
pub(crate) struct WabtFormatter {
    current_indent: usize,
}

impl WabtFormatter {
    #[allow(clippy::new_without_default)]
    pub fn new() -> WabtFormatter {
        WabtFormatter { current_indent: 0 }
    }
}

impl Formatter for WabtFormatter {
    #[inline]
    fn begin_array<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.current_indent += 1;
        if self.current_indent == 2 {
            writer.write_all(b"[\n  ")
        } else {
            writer.write_all(b"[")
        }
    }

    #[inline]
    fn end_array<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.current_indent -= 1;
        writer.write_all(b"]")
    }

    #[inline]
    fn begin_array_value<W>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        if first {
            Ok(())
        } else if self.current_indent == 2 {
            writer.write_all(b", \n  ")
        } else {
            writer.write_all(b", ")
        }
    }

    #[inline]
    fn begin_object<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.current_indent += 1;
        writer.write_all(b"{")
    }

    #[inline]
    fn end_object<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.current_indent -= 1;
        writer.write_all(b"}")
    }

    #[inline]
    fn begin_object_key<W>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        if first {
            Ok(())
        } else if self.current_indent == 1 {
            writer.write_all(b",\n ")
        } else {
            writer.write_all(b", ")
        }
    }

    #[inline]
    fn begin_object_value<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        writer.write_all(b": ")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use assert_json_diff::*;
    use serde_json::json;

    #[test]
    fn test_const_i32() {
        assert_json_eq!(
            WastJsonConst::I32 { value: -1 },
            json!({ "type": "i32", "value": "4294967295" })
        );
    }

    #[test]
    fn test_const_i64() {
        assert_json_eq!(
            WastJsonConst::I64 { value: -1 },
            json!({ "type": "i64", "value": "18446744073709551615" })
        );
    }

    #[test]
    fn test_const_f32() {
        assert_json_eq!(
            WastJsonConst::F32 {
                value: Float32 { bits: 0xc0400000 }
            },
            json!({ "type": "f32", "value": "3225419776" })
        );
    }

    #[test]
    fn test_const_f64() {
        assert_json_eq!(
            WastJsonConst::F64 {
                value: Float64 {
                    bits: 0xc008000000000000
                }
            },
            json!({ "type": "f64", "value": "13837309855095848960" })
        );
    }

    #[test]
    fn test_const_v128() {
        assert_json_eq!(
            WastJsonConst::V128(V128Const::F32([
                Float32 { bits: 1 },
                Float32 { bits: 2 },
                Float32 { bits: 3 },
                Float32 { bits: 4 },
            ])),
            json!({ "type": "v128", "lane_type": "f32", "value": ["1", "2", "3", "4"] })
        );
    }

    #[test]
    fn test_const_funcref() {
        assert_json_eq!(
            WastJsonConst::FuncRef {
                value: RefIndex::Index(1)
            },
            json!({ "type": "funcref", "value": "1" })
        );
    }

    #[test]
    fn test_const_funcref_null() {
        assert_json_eq!(
            WastJsonConst::FuncRef {
                value: RefIndex::Null
            },
            json!({ "type": "funcref", "value": "null" })
        );
    }

    #[test]
    fn test_const_externref() {
        assert_json_eq!(
            WastJsonConst::ExternRef {
                value: RefIndex::Index(1),
            },
            json!({ "type": "externref", "value": "1" })
        );
    }

    #[test]
    fn test_const_externref_null() {
        assert_json_eq!(
            WastJsonConst::ExternRef {
                value: RefIndex::Null
            },
            json!({ "type": "externref", "value": "null" })
        );
    }

    #[test]
    fn test_pattern_f64_nan_arithmtic() {
        assert_json_eq!(
            WastJsonExpected::F64 {
                value: NanPattern::ArithmeticNan
            },
            json!({ "type": "f64", "value": "nan:arithmetic" })
        );
    }
}
