=======
Parsing
=======

.. currentmodule:: qiskit_qasm2

This package currently contains two public functions, both of which create a
:class:`~qiskit.circuit.QuantumCircuit` from an OpenQASM 2 program. :func:`load` takes a filename,
while :func:`loads` takes the program itself as a string.  Their internals are very similar, so both
offer the same API.

Both functions can optionally take an `include_path` argument, which contains the directories that
should be searched (in order) to find files specified by ``include`` statements in OpenQASM 2.  The
include ``qelib1.inc`` is treated as a system include; this will always be found, regardless of the
import settings. This helps ensure correct mapping to gates that Qiskit has builtin definitions for.

.. autofunction:: load

.. autofunction:: loads

In cases where the lexer or parser fails due to an invalid OpenQASM 2 file, the conversion functions
will raise an error with a message explaining what the failure is, and where in the file it
occurred.

.. autoexception:: QASM2ParseError
