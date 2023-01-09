use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pyo3::Python;

mod bytecode;
mod error;
mod expr;
mod lex;
mod parse;

/// Create a bytecode iterable from a string containing an OpenQASM 2 program.  The iterable will
/// lex and parse the source lazily; evaluating OpenQASM 2 statements as required, without loading
/// the entire token and parse tree into memory at once.
#[pyfunction]
fn bytecode_from_string(_py: Python<'_>, string: String) -> bytecode::BytecodeIterator {
    bytecode::BytecodeIterator::new(lex::TokenStream::from_string(string))
}

/// Create a bytecode iterable from a path to a file containing an OpenQASM 2 program.  The
/// iterable will lex and parse the source lazily; evaluating OpenQASM 2 statements as required,
/// without loading the entire token and parse tree into memory at once.
#[pyfunction]
fn bytecode_from_file(
    _py: Python<'_>,
    path: std::ffi::OsString,
) -> PyResult<bytecode::BytecodeIterator> {
    Ok(bytecode::BytecodeIterator::new(
        lex::TokenStream::from_path(path)?,
    ))
}

create_exception!(
    core,
    QASM2ParseError,
    PyException,
    "An error raised during parsing of OpenQASM 2 programs."
);

/// An interface to the Rust components of the parser stack, and the types it uses to represent the
/// output.  The principal entry points for Python are :func:`bytecode_from_string` and
/// :func:`bytecode_from_file`, which produce iterables of :class:`Bytecode` objects.
#[pymodule]
fn core(py: Python<'_>, module: &PyModule) -> PyResult<()> {
    module.add_class::<bytecode::OpCode>()?;
    module.add_class::<bytecode::UnaryOpCode>()?;
    module.add_class::<bytecode::BinaryOpCode>()?;
    module.add_class::<bytecode::Bytecode>()?;
    module.add_class::<bytecode::ExprConstant>()?;
    module.add_class::<bytecode::ExprArgument>()?;
    module.add_class::<bytecode::ExprUnary>()?;
    module.add_class::<bytecode::ExprBinary>()?;
    module.add("QASM2ParseError", py.get_type::<QASM2ParseError>())?;
    module.add_function(wrap_pyfunction!(bytecode_from_string, module)?)?;
    module.add_function(wrap_pyfunction!(bytecode_from_file, module)?)?;
    Ok(())
}
