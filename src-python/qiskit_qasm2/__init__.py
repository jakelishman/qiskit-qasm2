import os
from pathlib import Path
from typing import Iterable, Union, Optional, Literal

with open(Path(__file__).parent / "VERSION", "r", encoding="utf-8") as _version_file:
    __version__ = _version_file.read().strip()

from . import core as _core, parse as _parse  # pylint: disable=no-name-in-module
from .core import QASM2ParseError
from .parse import CustomInstruction, QISKIT_CUSTOM_INSTRUCTIONS


def _normalize_path(path: Union[str, os.PathLike]) -> str:
    """Normalise a given path into a path-like object that can be passed to Rust.

    Ideally this would be something that we can convert to Rust's `OSString`, but in practice,
    Python uses `os.fsencode` to produce a `bytes` object, but this doesn't map especially well.
    """
    path = Path(path).expanduser().absolute()
    if not path.exists():
        raise FileNotFoundError(str(path))
    return str(path)


def loads(
    string: str,
    include_path: Iterable[Union[str, os.PathLike]] = (".",),
    custom_instructions: Iterable[CustomInstruction] = (),
):
    """Parse an OpenQASM 2 program from a string into a :class:`~qiskit.circuit.QuantumCircuit`.

    :param string: The OpenQASM 2 program in a string.
    :type string: str

    :return: A circuit object representing the same OpenQASM 2 program.
    :rtype: ~qiskit.circuit.QuantumCircuit
    """
    custom_instructions = list(custom_instructions)
    return _parse.from_bytecode(
        _core.bytecode_from_string(
            string,
            [_normalize_path(path) for path in include_path],
            [
                _core.CustomInstruction(x.name, x.n_params, x.n_qubits, x.builtin)
                for x in custom_instructions
            ],
        ),
        custom_instructions,
    )


def load(
    filename: Union[str, os.PathLike],
    include_path: Iterable[Union[str, os.PathLike]] = (".",),
    include_input_directory: Optional[Literal["append", "prepend"]] = "append",
    custom_instructions: Iterable[CustomInstruction] = (),
):
    """Parse an OpenQASM 2 program from a file into a :class:`~qiskit.circuit.QuantumCircuit`.  The
    given path should be ASCII or UTF-8 encoded, and contain the OpenQASM 2 program.

    :param filename: A filename for a file that contains an OpenQASM 2 program.
    :type filename: str

    :param include_file_directory: Whether to add the directory of the input file to the
        `include_path`, and if so, whether to `append` it to search last, or `prepend` it to search
        first.  Pass `None` to suppress adding this directory entirely.
    :type include_file_directory: ``None``, ``"append"`` or ``"prepend"``.

    :return: A circuit object representing the same OpenQASM 2 program.
    :rtype: ~qiskit.circuit.QuantumCircuit
    """
    filename = Path(filename)
    include_path = [_normalize_path(path) for path in include_path]
    if include_input_directory == "append":
        include_path.append(str(filename.parent))
    elif include_input_directory == "prepend":
        include_path.insert(0, str(filename.parent))
    elif include_input_directory is not None:
        raise ValueError(
            f"unknown value for include_input_directory: '{include_input_directory}'."
            " Valid values are '\"append\"', '\"prepend\"' and 'None'."
        )
    custom_instructions = tuple(custom_instructions)
    return _parse.from_bytecode(
        _core.bytecode_from_file(
            _normalize_path(filename),
            include_path,
            [
                _core.CustomInstruction(x.name, x.n_params, x.n_qubits, x.builtin)
                for x in custom_instructions
            ],
        ),
        custom_instructions,
    )
