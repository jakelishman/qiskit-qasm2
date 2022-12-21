use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pyo3::Python;

mod bytecode;
mod error;
mod expr;
mod lex;
mod parse;

#[pyfunction]
fn bytecode_from_string(_py: Python<'_>, string: String) -> bytecode::ByteCodeStringIterator {
    bytecode::ByteCodeStringIterator::new(lex::TokenStream::from_string(string))
}

#[pyfunction]
fn bytecode_from_file(
    _py: Python<'_>,
    path: std::ffi::OsString,
) -> PyResult<bytecode::ByteCodeFileIterator> {
    Ok(bytecode::ByteCodeFileIterator::new(
        lex::TokenStream::from_path(&path)?,
    ))
}

create_exception!(
    core,
    QASM2ParseError,
    PyException,
    "An error raised during parsing of OpenQASM 2 programs."
);

#[pymodule]
fn core(py: Python<'_>, module: &PyModule) -> PyResult<()> {
    module.add_class::<bytecode::OpCode>()?;
    module.add_class::<bytecode::ByteCode>()?;
    module.add("QASM2ParseError", py.get_type::<QASM2ParseError>())?;
    module.add_function(wrap_pyfunction!(bytecode_from_string, module)?)?;
    module.add_function(wrap_pyfunction!(bytecode_from_file, module)?)?;
    Ok(())
}
