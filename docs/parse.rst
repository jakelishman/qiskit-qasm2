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

In cases where the lexer or parser fails due to an invalid OpenQASM 2 file, the conversion functions
will raise an error with a message explaining what the failure is, and where in the file it
occurred.

.. autoexception:: QASM2ParseError
