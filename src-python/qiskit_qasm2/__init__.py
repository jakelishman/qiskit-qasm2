from pathlib import Path as _Path

with open(_Path(__file__).parent / "VERSION", "r", encoding="utf-8") as _version_file:
    __version__ = _version_file.read().strip()

from . import core, parse
from .core import QASM2ParseError


def loads(string):
    """Parse an OpenQASM 2 program from a string into a :class:`~qiskit.circuit.QuantumCircuit`.

    :param string: The OpenQASM 2 program in a string.
    :type string: str

    :raises QASM2ParseError: If the OpenQASM 2 program is invalid, or otherwise cannot be converted
        to Qiskit format.

    :return: A circuit object representing the same OpenQASM 2 program.
    :rtype: ~qiskit.circuit.QuantumCircuit
    """
    return parse.from_bytecode(core.bytecode_from_string(string))


def load(path):
    """Parse an OpenQASM 2 program from a file into a :class:`~qiskit.circuit.QuantumCircuit`.  The
    given path should be ASCII or UTF-8 encoded, and contain the OpenQASM 2 program.

    :param path: A filename for a file that contains an OpenQASM 2 program.
    :type path: str

    :raises QASM2ParseError: If the OpenQASM 2 program is invalid, or otherwise cannot be converted
        to Qiskit format.

    :return: A circuit object representing the same OpenQASM 2 program.
    :rtype: ~qiskit.circuit.QuantumCircuit
    """
    return parse.from_bytecode(core.bytecode_from_file(path))
