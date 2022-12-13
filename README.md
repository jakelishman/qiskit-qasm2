# Importer from OpenQASM 2 to Qiskit

[![License](https://img.shields.io/github/license/jakelishman/qiskit-qasm2.svg?style=popout-square)](https://opensource.org/licenses/Apache-2.0)<!--- long-description-skip-begin -->[![Release](https://img.shields.io/github/release/jakelishman/qiskit-qasm2.svg?style=popout-square)](https://github.com/jakelishman/qiskit-qasm2/releases)[![Downloads](https://img.shields.io/pypi/dm/qiskit-qasm2.svg?style=popout-square)](https://pypi.org/project/qiskit-qasm2/)<!--- long-description-skip-end -->

This repository provides the Python package `qiskit_qasm2`, which provides a
fast parser of OpenQASM 2 into Qiskit's `QuantumCircuit`.

This package was mostly an excuse for me to learn a bit more about how lexers
are written at a low level.  This is why the Rust crate doesn't use any
lexer-generation libraries.


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
