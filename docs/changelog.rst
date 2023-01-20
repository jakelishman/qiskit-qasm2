=========
Changelog
=========

Unreleased
==========

* Added support for specifying custom instructions, both builtin and requiring definitions
  inside the OpenQASM 2 file.  This is the `custom_instructions` parameter to :func:`.load`
  and :func:`.loads`.

* Added a data element :data:`.QISKIT_CUSTOM_INSTRUCTIONS` that can be passed to
  `custom_instructions` to cause :mod:`qiskit_qasm2` to :ref:`mostly emulate the behaviour of the
  Qiskit methods <qiskit-compatibility>` :meth:`QuantumCircuit.from_qasm_str()
  <qiskit.circuit.QuantumCircuit.from_qasm_str>` and
  :meth:`~qiskit.circuit.QuantumCircuit.from_qasm_file`.

0.2.0 (2023-01-09)
==================

* Added support for ``include`` statements, with the option to set the `include_path` in both
  :func:`.load` and :func:`.loads`.

* Swapped the internal Rust implementation to use ``dyn BufRead`` instead of being generic.  This
  reduced compile times and duplication of internal boiler plate in the PyO3 bindings.

0.1.0 (2023-01-08)
==================

* Initial release for CPython 3.8 to 3.11, on Linux i686, Linux x86_64, macOS x86_64, Windows x64
  and Windows x32.
