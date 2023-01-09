# Importer from OpenQASM 2 to Qiskit

[![License](https://img.shields.io/github/license/jakelishman/qiskit-qasm2.svg?style=flat)](https://opensource.org/licenses/Apache-2.0)<!--- long-description-skip-begin --> [![Release](https://img.shields.io/github/release/jakelishman/qiskit-qasm2.svg?style=flat)](https://github.com/jakelishman/qiskit-qasm2/releases) [![Downloads](https://img.shields.io/pypi/dm/qiskit-qasm2.svg?style=flat)](https://pypi.org/project/qiskit-qasm2/) [![Coverage Status](https://coveralls.io/repos/github/jakelishman/qiskit-qasm2/badge.svg?branch=main)](https://coveralls.io/github/jakelishman/qiskit-qasm2?branch=main)<!--- long-description-skip-end -->

This repository provides the Python package `qiskit_qasm2`, which provides a
fast parser of OpenQASM 2 into Qiskit's `QuantumCircuit`.  It is often 10x or
more faster than Qiskit's native parser.  The API is simple:

- `qiskit_qasm2.load` takes a filename, and returns `QuantumCircuit`;
- `qiskit_qasm2.loads` takes an OpenQASM 2 program in a string, and returns
  `QuantumCircuit`.

For example:
```python
import qiskit_qasm2
program = """
    OPENQASM 2.0;
    include "qelib1.inc";
    qreg q[2];
    creg c[2];

    h q[0];
    cx q[0], q[1];

    measure q -> c;
"""
qiskit_qasm2.loads(program).draw()
```
```text
     ┌───┐     ┌─┐
q_0: ┤ H ├──■──┤M├───
     └───┘┌─┴─┐└╥┘┌─┐
q_1: ─────┤ X ├─╫─┤M├
          └───┘ ║ └╥┘
c: 2/═══════════╩══╩═
                0  1
```

## Features

The parser supports almost all of [the OpenQASM 2
specification](https://arxiv.org/abs/1707.03429v2), including:

- register definitions and usage (`qreg` and `creg`);
- the `qelib1.inc` include, precisely as described in the paper;
- custom `gate` and `opaque` declarations;
- gate, measurement and reset broadcasting;
- conditioned gate applications, measurements and reset;
- constant folding with the scientific calculator functions in gate parameter
  lists;
- mathematical expressions on parameters within custom gate bodies.

There currently is no support for general `include` statements, but this is in
progress.

Qiskit itself adds in some non-paper gate definitions when it sees the
`qelib1.inc` include, and treats a non-unitary operation called `delay`
magically.  This parser does not make these extra-spec additions.


## Installation

Install the latest release of the `qiskit_qasm2` package from pip:

```bash
pip install qiskit_qasm2
```


## Developing

If you're looking to contribute to this project, please first read
[our contributing guidelines](CONTRIBUTING.md).

Set up your development environment by installing the development requirements
with pip:

```bash
pip install -r requirements-dev.txt tox
```

This installs a few more packages than the dependencies of the package at
runtime, because there are some tools we use for testing also included, such as
`tox` and `pytest`.

After the development requirements are installed, you can install an editable
version of the package with

```bash
pip install -e .
```

After this, any changes you make to the library code will immediately be present
when you open a new Python interpreter session.

This package was mostly an excuse for me to learn a bit more about how lexers
are written at a low level.  This is why the Rust crate doesn't use any
lexer-generation libraries.  You can read a bit more about the architecture and
some of the design decisions in [the developer section of the
documentation](https://jakelishman.github.io/qiskit-qasm2/dev.html).


### Building documentation

After the development requirements have been installed, the command

```bash
tox -e docs
```

will build the HTML documentation, and place it in `docs/_build/html`.  The
documentation state of the `main` branch of this repository is published to
https://jakelishman.github.io/qiskit-qasm2.


### Code style and linting

The Python components of this repository are formatted using `black`.  You can
run this on the required files by running

```bash
tox -e black
```

The full lint suite can be run with

```bash
tox -e lint
```


## License

This project is licensed under [version 2.0 of the Apache License](LICENSE).
