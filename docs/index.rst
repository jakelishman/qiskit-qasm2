===========================
OpenQASM 2 Tools for Qiskit
===========================

.. module:: qiskit_qasm2

This project is an exploratory package for interoperating between Qiskit and OpenQASM 2. It
currently offers the functions :func:`.load` and :func:`.loads`, which are significantly faster
(often 10x or more) drop-in replacements for the Qiskit methods :meth:`QuantumCircuit.from_qasm_file
<qiskit.circuit.QuantumCircuit.from_qasm_file>` and :meth:`QuantumCircuit.from_qasm_str
<qiskit.circuit.QuantumCircuit.from_qasm_str>` respectively.  The core of these functions is a
custom parser written in Rust.

In the future, this package may add exploratory new methods to replace :meth:`QuantumCircuit.qasm
<qiskit.circuit.QuantumCircuit.qasm>` as well.  There are various historical inefficiencies and
architectural difficulties in the current implementation, and this package provides a testbed free
of Qiskit's deprecation policy to help develop a new interface.  The experience may go on to inform
changes to Qiskit's OpenQASM 3 support as well.

.. toctree::

   parse.rst
   dev.rst
