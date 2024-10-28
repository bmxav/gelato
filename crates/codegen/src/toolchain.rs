use thiserror::Error;

use std::io::{Read, Write};
use std::process::{Command, ExitStatus, Stdio};
use std::string::FromUtf8Error;

#[derive(Error, Debug)]
pub enum ToolchainError {
    #[error("failed to run process '{name}': {details}")]
    Generic {
        name: String,
        details: String
    },
    #[error("process error")]
    ProcessError(#[from] std::io::Error),
    #[error("process failed with exit status {0}")]
    ExitStatusError(ExitStatus),
    #[error("unicode error")]
    UnicodeError(#[from] FromUtf8Error),
}

pub fn go_fmt(source: &mut impl Read) -> Result<String, ToolchainError> {
    let mut process = Command::new("gofmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let mut buf = Vec::new();
    source.read_to_end(&mut buf)?;

    process.stdin
        .as_mut()
        .ok_or(ToolchainError::Generic {
            name: "gofmt".to_string(),
            details: "failed to capture stdin".to_string()
        })?
        .write_all(&buf)?;

    let output = process.wait_with_output()?;
    if !output.status.success() {
        return Err(ToolchainError::ExitStatusError(output.status))
    }

    let formatted_text = String::from_utf8(output.stdout)?;
    Ok(formatted_text)
}
