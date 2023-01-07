===========
Development
===========

This page contains documentation on the development of the package, including the architecture and
design decisions behind the package.

Architecture
============

The parser is split into two components: a Rust core that implements all the lexing logic and the
vast majority of the parsing logic; and a Python interpreter for a small "bytecode" domain-specific
langauge the Rust component outputs.  The aim is that as little as possible should be done in Python
space, because Python interpretation is slow.

The Rust components produce a small Python interface using PyO3.  For the most part, the only API
surface between the Rust and Python components is a small DSL, bound using PyO3.  Custom iterator
objects are directly returned from Rust, which iterator over this DSL.  The Python components assume
that the bytecode produces a completely valid program; Rust is responsible for raising exceptions
because of an invalid OpenQASM 2 file, or a file that cannot be converted to Qiskit format for any
reason.

Both lexing and parsing are done in Rust by hand.  This is largely because I wanted to do this for
my own experience---I do know about `nom <https://github.com/Geal/nom>`__.  The lexer uses a simple
`LL(1) <https://en.wikipedia.org/wiki/LL_parser>`__ tokenisation strategy for all symbols, numbers
and comments, and only changes behaviour when lexing a text-like symbol.  Here, the entire
identifier-like symbol is read in, and the lexer then decides whether to emit a relevant keyword
token or arbitrary identifier token depeneding on the content.  Technically I suppose that means it
has arbitrary lookahead at that point.

The parser is principally a hand-written LL(1) `recursive-descent parser
<https://en.wikipedia.org/wiki/Recursive_descent_parser>`__.  The exception to this is
in expression contexts; here, a short-lived (also LL(1)) `operator-precedence parser
<https://en.wikipedia.org/wiki/Operator-precedence_parser>`__ is spawned using the same token stream
to parse a single expression.

Gate definitions are handled by storing the bytecode for that gate inside a Qiskit
:class:`~qiskit.circuit.Gate` object, and having its :meth:`~qiskit.circuit.Gate._define` method
contain the very stripped-down version of the bytecode interpreter needed to evaluate this subset of
the code.  This lets us build the gate objects lazily; when we place calls to these gates into the
circuit, we don't need to evaluate their definitions until the user actually calls for it to happen.
Unfortunately the PyO3 types in the bytecode interpreter aren't inherently pickleable, so to handle
this, we have to eagerly create the definition and throw away the bytecode at that point.



Testing
=======

The vast majority of the tests only use :func:`.loads`.  This is mostly deliberate.  Rust has to be
responsible for opening the files when using :func:`.load`, so we can't use any of Python's
in-memory file-like objects such as ``io.TextIO``.  When we want to test :func:`.load`, we have to
have an actual file object.  We could parametrise by having every single test case in a separate
file, and for the :func:`.loads` test we read in the whole string first.  I don't like this form,
though, beacuse it makes the actual test hard to read; the OpenQASM code ends up in a different
place to the Qiskit generating code, making it hard to quickly verify what is happening.

Instead, I mostly use :func:`.loads` as the test.  The implementation in Rust is generic over both,
and both are immediately abstracted into a single ``impl BufRead`` in the lexer, so should have next
to no differences.  The tests of the examples in `the arXiv paper
<https://arxiv.org/abs/1707.03429v2>`__ are parametrised over both as a check, but all the rest only
use :func:`.loads` to make the tests more readable.


Coverage
========

Code coverage metrics for both the Python and Rust components can be generated with the tox
environment ``coverage``.  Additionally, after this run, one can also generate a set of HTML pages
graphical illustrating the coverage by running the ``coverage-html`` environment, such as by

.. code-block:: bash

   tox -e coverage,coverage-html

These environments have some additional non-Python dependencies that must be installed separately.
These are the ``llvm-tools-preview`` component for rustup, |grcov|_, and |lcov|_.

.. |grcov| replace:: Mozilla's ``grcov`` tool for aggregating coverage data from instrumented Rust code
.. _grcov: https://github.com/mozilla/grcov
.. |lcov| replace:: the ``lcov`` package
.. _lcov: https://github.com/linux-test-project/lcov

* ``llvm-tools-preview`` can be installed using `rustup <https://rustup.rs/>`__ by running

  .. code-block:: bash

    rustup component add llvm-tools-preview

* ``grcov`` is most easily installed by running

  .. code-block:: bash

    cargo install grcov

* The ``lcov`` package (which provides the binaries ``lcov`` and ``genhtml``) is likely available
  through your system package manager, if on Linux or Mac.  For example, on Ubuntu it can be
  installed with

  .. code-block:: bash

    sudo apt install lcov

  and on Mac via Homebrew it can be installed with

  .. code-block:: bash

    brew install lcov

After the ``coverage-html`` environment has been successfully executed, one can open the generated
HTML coverage information by opening the file ``coverage/index.html``.  The raw coverage information
file (in LCOV format) will be ``coverage.info`` in the repository root.

.. note::

  Running the ``coverage`` tox environment causes the compiled Rust code in the working directory
  for editable installs to be recompiled and instrumented for profiling data.  You might want to
  manually rebuild the Rust extension module with

  .. code-block:: bash

    python setup.py build_rust --inplace [--release]

  after using the ``coverage`` job, or all your uses of the compiled module will continue generating
  individual coverage data.
