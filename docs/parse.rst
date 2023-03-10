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

By default, the two loaders operate in a permissive mode.  This allows some syntactical niceties
that do not change the meaning of OpenQASM 2 programs, but are technically against the
specification.  This includes not *requiring* the version statement, allowing empty statements
(often seen as extraneous semicolons), and allowing trailing commas in various lists.  You can swap
to a precise implementation of the spec by making the `strict` parameter to both true.

You can extend the OpenQASM 2 language by supplying constructors for instructions whose definitions
cannot be expressed in OpenQASM 2, and by supplying extra functions that should be available to the
"scientific calculator" functionality.

Extra instructions are supplied by passing an iterable of information on custom instructions
as the argument `custom_instructions`.  In files that have compatible definitions for these
instructions, the given `constructor` will be used in place of whatever other handling
:mod:`qiskit_qasm2` would have done.  These instructions may optionally be marked as `builtin`, which
causes them to not require an ``opaque`` or ``gate`` declaration, but they will silently ignore a
compatible declaration.  Either way, it is an error to provide a custom instruction that has a
different number of parameters or qubits as a defined instruction in a parsed program.  Each element
of the argument iterable should be a particular data class:

.. autoclass:: CustomInstruction

Further scientific-calculator functions are supplied by providing an iterable of information on the
desired function, and a Python implementation of it.  You must supply the name, number of
floating-point parameters, and the function itself.  The function should take in that many
floating-point values as separate arguments, and return a float.  The information is supplied as a
particular data class:

.. autoclass:: CustomClassical

In cases where the lexer or parser fails due to an invalid OpenQASM 2 file, the conversion functions
will raise an error with a message explaining what the failure is, and where in the file it
occurred.

.. autoexception:: QASM2ParseError


.. _qiskit-compatibility:

Qiskit Compatibility
====================

Qiskit's :meth:`QuantumCircuit.from_qasm_str() <qiskit.circuit.QuantumCircuit.from_qasm_str>` and
:meth:`~qiskit.circuit.QuantumCircuit.from_qasm_file` have a few additions on top of the raw
specification, as Qiskit originally tried to use OpenQASM 2 as a sort of serialisation format, and
expanded their behaviour as Qiskit expanded.  This parser under all its defaults implements the
specification more precisely.

In particular, in the Qiskit importers:

* the allowed grammar is effectively our `strict` mode; Qiskit is quite inflexible.

* the `include_path` is:
    1. ``<qiskit>/qasm/libs``, where ``<qiskit>`` is the root of the installed ``qiskit`` package;
    2. the current working directory.

* there are additional instructions defined in ``qelib1.inc``:
   ``u0(gamma) a``
      This is ambiguous.  Its provided definition suggests it is a no-op and that the `gamma`
      parameter is ignored, but a comment above it describes it as a delay for `gamma` times the
      length of the shortest single-qubit gate.  In compatibility mode, we require `gamma` to be an
      integer, and set its definition to that many Qiskit :class:`~qiskit.circuit.library.IGate`\ s.

   ``u(theta, phi, lambda) a``
      A synonym for the OpenQASM 2 builtin gate ``U``.  Corresponds to
      :class:`~qiskit.circuit.library.UGate`.

   ``p(lambda) a``
      A synonym for the paper's ``u1``.  Corresponds to :class:`~qiskit.circuit.library.PhaseGate`,
      which is a synonym of :class:`~qiskit.circuit.library.U1Gate`.

   ``sx a``
      :math:`\sqrt X` gate, corresponding to :class:`~qiskit.circuit.library.SXGate`.

   ``sxdg a``
      :math:`\sqrt X^\dagger` gate, corresponding to :class:`~qiskit.circuit.library.SXdgGate`.

   ``swap a, b``
      The swap gate, corresponding to :class:`~qiskit.circuit.library.SwapGate`.

   ``cswap a, b, c``
      The controlled swap gate, corresponding to :class:`~qiskit.circuit.library.CSwapGate`.

   ``crx(theta) a, b``
      Controlled rotation around the :math:`X` axis. Corresponds to
      :class:`~qiskit.circuit.library.CRXGate`.

   ``cry(theta) a, b``
      Controlled rotation around the :math:`Y` axis. Corresponds to
      :class:`~qiskit.circuit.library.CRYGate`.

   ``cp(lambda) a, b``
      Controlled phase gate, which is a synonym for the paper gate ``cu1``. Corresponds to
      :class:`~qiskit.circuit.library.CPhaseGate`.

   ``csx a, b``
      Controlled :math:`\sqrt X` gate, corresponding to :class:`~qiskit.circuit.library.CSXGate`.

   ``cu(theta, phi, lambda, gamma) c, t``
      The four-parameter version of a controlled-:math:`U`, corresponding to
      :class:`~qiskit.circuit.library.CUGate`.

   ``rxx(theta) a, b``
      Two-qubit rotation arond the :math:`XX` axis, corresponding to
      :class:`~qiskit.circuit.library.RXXGate`.

   ``rzz(theta) a, b``
      Two-qubit rotation arond the :math:`ZZ` axis, corresponding to
      :class:`~qiskit.circuit.library.RZZGate`.

   ``rccx a, b, c``
      The double-controlled :math:`X` gate, but with relative phase differences over the standard
      Toffoli gate.  This *should* correspond to the Qiskit gate
      :class:`~qiskit.circuit.library.RCCXGate`, but the Qiskit converter won't actually output this
      type.

   ``rc3x a, b, c, d``
      The triple-controlled :math:`X` gate, but with relative phase differences over the standard
      definition.  *Should* correspond to :class:`~qiskit.circuit.library.RC3XGate`.

   ``c3x a, b, c, d``
      The triple-controlled :math:`X` gate, corresponding to :class:`~qiskit.circuit.library.C3XGate`.

   ``c3sqrtx a, b, c, d``
      The triple-controlled :math:`\sqrt X` gate.  *Should* correspond to
      :class:`~qiskit.circuit.library.C3SXGate`.

   ``c4x a, b, c, d, e``
      The quadruple-controlled :math:`X` gate.  *Should* correspond to
      :class:`~qiskit.circuit.library.C4XGate`.

* if *any* ``opaque`` or ``gate`` definition is given for the name ``delay``, they will attempt to
  output a :class:`~qiskit.circuit.Delay` instruction at each call.  To function, this expects a
  definition compatible with ``opaque delay(t) q;``, where the time ``t`` is given in units of
  ``dt``.  The importer will raise an error on calls to the instruction if there are actually not
  exactly one parameter and one qubit, or if the parameter is not integer-valued.

* the additional scientific-calculator functions ``asin``, ``acos`` and ``atan`` are available.

You can emulate this behaviour in :func:`load` and :func:`loads` by setting `include_path`
appropriately (try inspecting the variable ``qiskit.__file__`` to find the installed location), and
by passing a list of :class:`CustomInstruction` instances for each of the custom gates you care
about.  To make things easier, we make available one tuple containing all the above instructions
(using the correspondences that Qiskit forgets as well) that you can supply to
`custom_instructions`, and one containing the additional scientific-calculator functions.

.. py:data:: QISKIT_CUSTOM_INSTRUCTIONS

   A tuple containing the extra `custom_instructions` that Qiskit's built-in converters use if
   ``qelib1.inc`` is included, and there is any definition of a ``delay`` instruction.  The gates
   in the paper version of ``qelib1.inc`` and ``delay`` all require a compatible declaration
   statement to be present within the OpenQASM 2 program, but Qiskit's additions are all marked as
   builtins since they are not actually present in any include file this parser sees.

.. py:data:: QISKIT_CUSTOM_CLASSICAL

   A tuple containing the extra `custom_classical` functions that Qiskit's built-in converters use
   beyond those specified by the paper.  This is the three basic inverse trigonometric functions.

On *all* the gates defined in Qiskit's version of ``qelib1.inc`` and the ``delay`` instruction, it
does not matter how the gates are actually defined and used, Qiskit will always attempt to output
its custom objects for them.  This can result in errors during the circuit construction, even after
a successful parse.  There is no way to emulate this buggy behaviour in :mod:`qiskit_qasm2`; only an
``include "qelib1.inc";`` statement or the `custom_instructions` argument can cause built-in Qiskit
instructions to be used, and the signatures of these match each other.

.. note::

   Circuits imported with :func:`load` and :func:`loads` with the above Qiskit-compability settings
   should compare equal to those created by Qiskit's importers, provided no non-``qelib1.inc``
   user gates are defined.  User-defined gates are handled slightly differently between this package
   and Qiskit, and while they should have equivalent :attr:`definition
   <qiskit.circuit.Instruction.definition>` fields on inspection, this package uses a custom class
   to lazily load the definition when it is requested (like most Qiskit objects), rather than
   eagerly creating it during the parse.  Qiskit's comparison rules for gates will see these two
   objects as unequal, although any pass through :func:`qiskit.transpile()
   <qiskit.compiler.transpile>` for a particular backend should produce the same output circuits.
