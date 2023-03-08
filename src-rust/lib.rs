use pyo3::prelude::*;
use pyo3::Python;

mod bytecode;
mod error;
mod expr;
mod lex;
mod parse;

/// Information about a custom instruction that Python space is able to construct to pass down to
/// us.
#[pyclass]
#[derive(Clone)]
pub struct CustomInstruction {
    pub name: String,
    pub n_params: usize,
    pub n_qubits: usize,
    pub builtin: bool,
}

#[pymethods]
impl CustomInstruction {
    #[new]
    fn __new__(name: String, n_params: usize, n_qubits: usize, builtin: bool) -> Self {
        Self {
            name,
            n_params,
            n_qubits,
            builtin,
        }
    }
}

/// Information about a custom classical function that should be defined in mathematical
/// expressions.
///
/// The given `callable` must be a Python function that takes `n_params` floats, and returns a
/// float.  The `name` is the identifier that refers to it in the OpenQASM 2 program.  This cannot
/// clash with any defined gates.
#[pyclass(text_signature = "(name, n_params, callable, /)")]
#[derive(Clone)]
pub struct CustomClassical {
    pub name: String,
    pub n_params: usize,
    pub callable: PyObject,
}

#[pymethods]
impl CustomClassical {
    #[new]
    fn __new__(name: String, n_params: usize, callable: PyObject) -> Self {
        Self {
            name,
            n_params,
            callable,
        }
    }
}

/// Create a bytecode iterable from a string containing an OpenQASM 2 program.  The iterable will
/// lex and parse the source lazily; evaluating OpenQASM 2 statements as required, without loading
/// the entire token and parse tree into memory at once.
#[pyfunction]
fn bytecode_from_string(
    _py: Python<'_>,
    string: String,
    include_path: Vec<std::ffi::OsString>,
    custom_instructions: Vec<CustomInstruction>,
    custom_classical: Vec<CustomClassical>,
    strict: bool,
) -> PyResult<bytecode::BytecodeIterator> {
    bytecode::BytecodeIterator::new(
        lex::TokenStream::from_string(string),
        include_path.iter().map(|x| x.into()).collect(),
        &custom_instructions,
        &custom_classical,
        strict,
    )
}

/// Create a bytecode iterable from a path to a file containing an OpenQASM 2 program.  The
/// iterable will lex and parse the source lazily; evaluating OpenQASM 2 statements as required,
/// without loading the entire token and parse tree into memory at once.
#[pyfunction]
fn bytecode_from_file(
    _py: Python<'_>,
    path: std::ffi::OsString,
    include_path: Vec<std::ffi::OsString>,
    custom_instructions: Vec<CustomInstruction>,
    custom_classical: Vec<CustomClassical>,
    strict: bool,
) -> PyResult<bytecode::BytecodeIterator> {
    bytecode::BytecodeIterator::new(
        lex::TokenStream::from_path(path)?,
        include_path.iter().map(|x| x.into()).collect(),
        &custom_instructions,
        &custom_classical,
        strict,
    )
}

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
    module.add_class::<bytecode::ExprCustom>()?;
    module.add_class::<CustomInstruction>()?;
    module.add_class::<CustomClassical>()?;
    module.add("QASM2ParseError", py.get_type::<error::QASM2ParseError>())?;
    module.add_function(wrap_pyfunction!(bytecode_from_string, module)?)?;
    module.add_function(wrap_pyfunction!(bytecode_from_file, module)?)?;
    Ok(())
}
