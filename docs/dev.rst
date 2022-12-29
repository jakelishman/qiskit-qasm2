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

Emitting bytecode from general symbolic expressions---those created during the parsing of gate
bodies defined by ``gate`` statements---blurs the lines between Python and Rust.  For now, Rust uses
PyO3 to manipulate Python objects directly to build the Qiskit
:class:`~qiskit.circuit.ParameterExpression`\ s directly from the expression trees, rather than
trying to convert the split tree/arena structure used within Rust into an interpretable Python
object.

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
