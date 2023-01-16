=======
Parsing
=======

.. currentmodule:: qiskit_qasm2

This package currently contains two public functions, both of which create a
:class:`~qiskit.circuit.QuantumCircuit` from an OpenQASM 2 program. :func:`load` takes a filename,
while :func:`loads` takes the program itself as a string.  Their internals are very similar, so both
offer the same API.

.. autofunction:: load

.. autofunction:: loads

Both of these loading functions also take an argument `include_path`, which is an iterable of
directory names to use when searching for files in ``include`` statements.  The directories are
tried from index 0 onwards, and the first match is used.  The import ``qelib1.inc`` is treated
specially; it is always found before looking in the include path, and contains exactly the content
of the `paper describing the OpenQASM 2 language <https://arxiv.org/abs/1707.03429>`__.  The gates
in this include file are mapped to standard gates provided by Qiskit.

In cases where the lexer or parser fails due to an invalid OpenQASM 2 file, the conversion functions
will raise an error with a message explaining what the failure is, and where in the file it
occurred.

.. autoexception:: QASM2ParseError
