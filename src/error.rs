use crate::json::WastJsonValueType;
use std::{fmt, io, path::Path, path::PathBuf, result};
use wast::token::Span;

pub(crate) type Result<T> = result::Result<T, Error>;

#[derive(thiserror::Error)]
#[error(transparent)]
pub struct Error {
    inner: Box<ErrorInner>,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind) -> Self {
        Self {
            inner: Box::new(ErrorInner {
                kind,
                span: None,
                path: None,
                pos: None,
                source_line: None,
            }),
        }
    }

    pub(crate) fn set_span(&mut self, offset: Span) {
        if self.inner.span.is_none() {
            self.inner.span = Some(offset);
        }
    }

    pub(crate) fn set_source(&mut self, path: &Path, source: &str) {
        self.inner.path = Some(path.to_owned());
        if let Some(span) = self.inner.span {
            let pos = span.linecol_in(source);
            self.inner.pos = Some(pos);
            self.inner.source_line = source.lines().nth(pos.0).map(|s| s.to_string());
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(thiserror::Error, Debug)]
struct ErrorInner {
    #[source]
    kind: ErrorKind,
    span: Option<Span>,
    path: Option<PathBuf>,
    pos: Option<(usize, usize)>,
    source_line: Option<String>,
}

impl fmt::Display for ErrorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref path) = self.path {
            write!(f, "{}", path.to_string_lossy())?;
            if let Some((line, col)) = self.pos {
                write!(f, ":{}:{}", line + 1, col + 1)?;
            }
            f.write_str(": ")?;
        }
        write!(f, "error: {}", self.kind)?;
        if let Some(ref source_line) = self.source_line {
            writeln!(f)?;
            f.write_str(source_line)?;
            if let Some((_, col)) = self.pos {
                writeln!(f)?;
                write!(f, "{}^", " ".repeat(col))?;
            }
        }
        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum ErrorKind {
    #[error("unknown module")]
    UnknownModule,
    #[error("unknown function export \"{0}\"")]
    UnknownFuncExport(String),
    #[error("unknown global export \"{0}\"")]
    UnknownGlobalExport(String),
    #[error("too {} {0}s to function. got {1}, expected {2}", if .1 < .2 { "few" } else { "many" })]
    WrongNumberOfTypes(TypeContext, usize, usize),
    #[error("type mismatch for {1} {2} of {0}. got {3}, expected {4}")]
    WrongType(
        String,
        TypeContext,
        usize,
        WastJsonValueType,
        WastJsonValueType,
    ),
    #[error("unsupported feature: {0}")]
    UnsupportedFeature(&'static str),
    #[error("{}", _0.message())]
    WastError(#[source] wast::Error),
    #[error("{0}")]
    IoError(#[source] io::Error),
    #[error("{0}")]
    JsonError(#[source] serde_json::Error),
    #[error("{0}")]
    BinaryReaderError(#[source] wasmparser::BinaryReaderError),
}

#[derive(Debug)]
pub enum TypeContext {
    Argument,
    Result,
}

impl fmt::Display for TypeContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Argument => "argument",
            Self::Result => "result",
        })
    }
}

impl From<wasmparser::BinaryReaderError> for Error {
    fn from(source: wasmparser::BinaryReaderError) -> Self {
        Error::new(ErrorKind::BinaryReaderError(source))
    }
}

impl From<wast::Error> for Error {
    fn from(source: wast::Error) -> Self {
        let span = source.span();
        let mut error = Error::new(ErrorKind::WastError(source));
        error.set_span(span);
        error
    }
}

impl From<io::Error> for Error {
    fn from(source: io::Error) -> Self {
        Error::new(ErrorKind::IoError(source))
    }
}

impl From<serde_json::Error> for Error {
    fn from(source: serde_json::Error) -> Self {
        Error::new(ErrorKind::JsonError(source))
    }
}

impl From<ErrorKind> for Error {
    fn from(source: ErrorKind) -> Self {
        Error::new(source)
    }
}
