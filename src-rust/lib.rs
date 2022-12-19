use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::Python;

mod error;
mod expr;
mod lex;
mod parse;

#[pyfunction]
fn bytecode_from_string(py: Python<'_>, string: String) -> PyResult<Vec<parse::ByteCode>> {
    match parse::parse(lex::TokenStream::from_string(string)) {
        Ok(internal_byte_code) => Ok(internal_byte_code.iter().map(|x| x.to_python(py)).collect()),
        Err(message) => Err(PyValueError::new_err(message)),
    }
}

#[pymodule]
fn core(_py: Python<'_>, module: &PyModule) -> PyResult<()> {
    module.add_class::<parse::OpCode>()?;
    module.add_class::<parse::ByteCode>()?;
    module.add_function(wrap_pyfunction!(bytecode_from_string, module)?)?;
    Ok(())
}
