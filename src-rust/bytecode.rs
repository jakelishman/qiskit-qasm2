use pyo3::prelude::*;

use crate::expr::{Expr, ExprArena};
use crate::lex;
use crate::parse;
use crate::QASM2ParseError;

#[pyclass(module = "qiskit_qasm2.core", frozen)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    Gate,
    ConditionedGate,
    Measure,
    ConditionedMeasure,
    Reset,
    ConditionedReset,
    Barrier,
    DeclareQreg,
    DeclareCreg,
    DeclareGate,
    EndDeclareGate,
    DeclareOpaque,
    SpecialInclude,
}

#[pyclass(module = "qiskit_qasm2.core", frozen)]
pub struct ByteCode {
    #[pyo3(get)]
    opcode: OpCode,
    #[pyo3(get)]
    operands: PyObject,
}

pub enum InternalByteCode {
    Gate {
        id: usize,
        parameters: Vec<f64>,
        qubits: Vec<usize>,
    },
    ConditionedGate {
        id: usize,
        parameters: Vec<f64>,
        qubits: Vec<usize>,
        creg: usize,
        value: usize,
    },
    Measure {
        qubit: usize,
        clbit: usize,
    },
    ConditionedMeasure {
        qubit: usize,
        clbit: usize,
        creg: usize,
        value: usize,
    },
    Reset {
        qubit: usize,
    },
    ConditionedReset {
        qubit: usize,
        creg: usize,
        value: usize,
    },
    Barrier {
        qubits: Vec<usize>,
    },
    DeclareQreg {
        name: String,
        size: usize,
    },
    DeclareCreg {
        name: String,
        size: usize,
    },
    DeclareGate {
        name: String,
        params: Vec<String>,
        n_qubits: usize,
    },
    GateInBody {
        id: usize,
        parameters: Vec<(Expr, ExprArena)>,
        qubits: Vec<usize>,
    },
    EndDeclareGate {},
    DeclareOpaque {
        name: String,
        n_params: usize,
        n_qubits: usize,
    },
    SpecialInclude {
        name: String,
    },
}

impl InternalByteCode {
    pub fn to_python(&self, py: Python, pyparams: &mut Vec<Py<PyAny>>) -> PyResult<ByteCode> {
        match self {
            InternalByteCode::Gate {
                id,
                parameters,
                qubits,
            } => Ok(ByteCode {
                opcode: OpCode::Gate,
                operands: (id, parameters, qubits).to_object(py),
            }),
            InternalByteCode::ConditionedGate {
                id,
                parameters,
                qubits,
                creg,
                value,
            } => Ok(ByteCode {
                opcode: OpCode::ConditionedGate,
                operands: (id, parameters, qubits, creg, value).to_object(py),
            }),
            InternalByteCode::Measure { qubit, clbit } => Ok(ByteCode {
                opcode: OpCode::Measure,
                operands: (qubit, clbit).to_object(py),
            }),
            InternalByteCode::ConditionedMeasure {
                qubit,
                clbit,
                creg,
                value,
            } => Ok(ByteCode {
                opcode: OpCode::ConditionedMeasure,
                operands: (qubit, clbit, creg, value).to_object(py),
            }),
            InternalByteCode::Reset { qubit } => Ok(ByteCode {
                opcode: OpCode::Reset,
                operands: (qubit,).to_object(py),
            }),
            InternalByteCode::ConditionedReset { qubit, creg, value } => Ok(ByteCode {
                opcode: OpCode::ConditionedReset,
                operands: (qubit, creg, value).to_object(py),
            }),
            InternalByteCode::Barrier { qubits } => Ok(ByteCode {
                opcode: OpCode::Reset,
                operands: (qubits,).to_object(py),
            }),
            InternalByteCode::DeclareQreg { name, size } => Ok(ByteCode {
                opcode: OpCode::DeclareQreg,
                operands: (name, size).to_object(py),
            }),
            InternalByteCode::DeclareCreg { name, size } => Ok(ByteCode {
                opcode: OpCode::DeclareCreg,
                operands: (name, size).to_object(py),
            }),
            InternalByteCode::DeclareGate {
                name,
                params,
                n_qubits,
            } => {
                let parameter_builder =
                    PyModule::import(py, "qiskit.circuit")?.getattr("Parameter")?;
                for param_name in params.iter() {
                    pyparams.push(parameter_builder.call1((param_name,))?.into());
                }
                Ok(ByteCode {
                    opcode: OpCode::DeclareGate,
                    operands: (name, pyparams.clone(), n_qubits).to_object(py),
                })
            }
            InternalByteCode::GateInBody {
                id,
                parameters,
                qubits,
            } => {
                let mut new_parameters = Vec::new();
                for (expr, arena) in parameters.iter() {
                    new_parameters.push(expr.to_python(py, arena, pyparams)?)
                }
                Ok(ByteCode {
                    // In Python space, we don't have to be worried about the types of the
                    // parameters changing here, so we can just use `OpCode::Gate` unlike in the
                    // internal byte code.
                    opcode: OpCode::Gate,
                    operands: (id, new_parameters, qubits).to_object(py),
                })
            }
            InternalByteCode::EndDeclareGate {} => {
                pyparams.clear();
                Ok(ByteCode {
                    opcode: OpCode::EndDeclareGate,
                    operands: ().to_object(py),
                })
            }
            InternalByteCode::DeclareOpaque {
                name,
                n_params,
                n_qubits,
            } => Ok(ByteCode {
                opcode: OpCode::DeclareOpaque,
                operands: (name, n_params, n_qubits).to_object(py),
            }),
            InternalByteCode::SpecialInclude { name } => Ok(ByteCode {
                opcode: OpCode::SpecialInclude,
                operands: (name,).to_object(py),
            }),
        }
    }
}

#[pyclass]
pub struct ByteCodeStringIterator {
    parser_state: parse::State<std::io::Cursor<String>>,
    called: bool,
    buffer: Vec<InternalByteCode>,
    buffer_used: usize,
    current_parameters: Vec<Py<PyAny>>,
}

impl ByteCodeStringIterator {
    pub fn new(tokens: lex::TokenStream<std::io::Cursor<String>>) -> Self {
        ByteCodeStringIterator {
            parser_state: parse::State::new(tokens),
            called: false,
            buffer: vec![],
            buffer_used: 0,
            current_parameters: vec![],
        }
    }
}

#[pymethods]
impl ByteCodeStringIterator {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __next__(&mut self, py: Python<'_>) -> PyResult<Option<ByteCode>> {
        if self.buffer_used >= self.buffer.len() {
            self.buffer.clear();
            self.buffer_used = 0;
            self.parser_state
                .parse_next(&mut self.buffer, !self.called)
                .map_err(QASM2ParseError::new_err)?;
            self.called = true;
        }
        if self.buffer.is_empty() {
            Ok(None)
        } else {
            self.buffer_used += 1;
            Ok(Some(
                self.buffer[self.buffer_used - 1].to_python(py, &mut self.current_parameters)?,
            ))
        }
    }
}

#[pyclass]
pub struct ByteCodeFileIterator {
    parser_state: parse::State<std::io::BufReader<std::fs::File>>,
    called: bool,
    buffer: Vec<InternalByteCode>,
    buffer_used: usize,
    current_parameters: Vec<Py<PyAny>>,
}

impl ByteCodeFileIterator {
    pub fn new(tokens: lex::TokenStream<std::io::BufReader<std::fs::File>>) -> Self {
        ByteCodeFileIterator {
            parser_state: parse::State::new(tokens),
            called: false,
            buffer: vec![],
            buffer_used: 0,
            current_parameters: vec![],
        }
    }
}

#[pymethods]
impl ByteCodeFileIterator {
    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __next__(&mut self, py: Python<'_>) -> PyResult<Option<ByteCode>> {
        if self.buffer_used >= self.buffer.len() {
            self.buffer.clear();
            self.buffer_used = 0;
            self.parser_state
                .parse_next(&mut self.buffer, !self.called)
                .map_err(QASM2ParseError::new_err)?;
            self.called = true;
        }
        if self.buffer.is_empty() {
            Ok(None)
        } else {
            self.buffer_used += 1;
            Ok(Some(
                self.buffer[self.buffer_used - 1].to_python(py, &mut self.current_parameters)?,
            ))
        }
    }
}
