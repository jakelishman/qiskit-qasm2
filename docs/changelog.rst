=========
Changelog
=========

0.5.1 (unreleased)
==================

* Relaxed the lexer to permit single-quoted paths in `include` statements when not running in
  `strict` mode.

* Improved error messages caused by failures during the lexing stage.

* Several internal refactorings to improve inter-component API boundaries.

0.5.0 (2023-03-08)
==================

* Added support for `custom_classical` functions to :func:`.load` and :func:`.loads`.  This allows
  per-run configuration of additional classical functions to make available both during constant
  folding and defined-gate argument evaluation.

* Added the Qiskit-compatibility data :data:`.QISKIT_CUSTOM_CLASSICAL` containing the additions that
  Qiskit's built-in parser uses during expression processing.

* Relaxed the expression parser to allow trailing commas in classical-function parameter lists.
  This previously was not allowed because the only defined classical functions by the specification
  were unary, and parsed rigidly.  This is forbidden in `strict` mode.

0.4.0 (2023-01-24)
==================

* Relaxed the parser to allow trailing commas in all list-like elements (of parameters and qubits),
  as a convenience in the default mode.

* Relaxed the parser to allow trailing semicolons after all statements, in particular allowing a
  semicolon after the body of a gate definition.

* Added a `strict` argument to :func:`.load` and :func:`.loads` that causes the parser to enforce
  the letter of the specification.  In particular, the two relaxations above are disabled, and the
  program *must* contain and start with a version statement.  This mode is more inline with Qiskit's
  parser.

0.3.2 (2023-01-23)
==================

* Added some missing gates to :data:`.QISKIT_CUSTOM_INSTRUCTIONS`; Qiskit's legacy importer
  made rather a lot of changes to the file as presented in the paper!

* Fixed incorrect gate creations when a strict subset of the ``qelib1.inc`` gates were overridden
  with custom constructors, or if any user gates were defined after all every gate in that include
  file was overridden.

0.3.1 (2023-01-20)
==================

* Use :class:`~qiskit.circuit.library.CU3Gate` for ``cu3`` in standard ``qelib1.inc`` imports.

* Fix the emitted gates for ``cu3`` and ``id`` in :data:`.QISKIT_CUSTOM_INSTRUCTIONS` so that the
  output matches Qiskit's loads more precisely.

0.3.0 (2023-01-20)
==================

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
