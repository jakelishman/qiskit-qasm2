import os
from pathlib import Path
from typing import Iterable, Union, Optional, Literal

with open(Path(__file__).parent / "VERSION", "r", encoding="utf-8") as _version_file:
    __version__ = _version_file.read().strip()

from . import core, parse
from .core import QASM2ParseError


def _normalize_path(path: Union[str, os.PathLike]) -> str:
    """Normalise a given path into a path-like object that can be passed to Rust.

    Ideally this would be something that we can convert to Rust's `OSString`, but in practice,
    Python uses `os.fsencode` to produce a `bytes` object, but this doesn't map especially well.
    """
    path = Path(path).expanduser().absolute()
    if not path.exists():
        raise FileNotFoundError(str(path))
    return str(path)


def loads(string: str, include_path: Iterable[Union[str, os.PathLike]] = (".",)):
    """Parse an OpenQASM 2 program from a string into a :class:`~qiskit.circuit.QuantumCircuit`.

    :param string: The OpenQASM 2 program in a string.
    :type string: str
    :param include_path: An iterable of directories to use when searching for include files.  These
        are tried in order, from index 0 onwards, and the first match is used.  The import
        `qelib1.inc` is treated as a system include, and will always be found regardless of the
        value of this argument.

        By default, this contains only the current working directory.
    :type include_path: Iterable[Union[str, os.PathLike]]

    :raises QASM2ParseError: If the OpenQASM 2 program is invalid, or otherwise cannot be converted
        to Qiskit format.

    :return: A circuit object representing the same OpenQASM 2 program.
    :rtype: ~qiskit.circuit.QuantumCircuit
    """
    return parse.from_bytecode(
        core.bytecode_from_string(string, [_normalize_path(path) for path in include_path])
    )


def load(
    filename: Union[str, os.PathLike],
    include_path: Iterable[Union[str, os.PathLike]] = (".",),
    include_input_directory: Optional[Literal["append", "prepend"]] = "append",
):
    """Parse an OpenQASM 2 program from a file into a :class:`~qiskit.circuit.QuantumCircuit`.  The
    given path should be ASCII or UTF-8 encoded, and contain the OpenQASM 2 program.

    :param filename: A filename for a file that contains an OpenQASM 2 program.
    :type filename: str

    :param include_path: An iterable of directories to use when searching for include files.  These
        are tried in order, from index 0 onwards, and the first match is used.  The import
        `qelib1.inc` is treated as a system include, and will always be found regardless of the
        value of this argument.

        By default, this contains only the current working directory.
    :type include_path: Iterable[Union[str, os.PathLike]]

    :param include_file_directory: Whether to add the directory of the input file to the
        `include_path`, and if so, whether to `append` it to search last, or `prepend` it to search
        first.  Pass `None` to suppress adding this directory entirely.
    :type include_file_directory: ``None``, ``"append"`` or ``"prepend"``.

    :raises QASM2ParseError: If the OpenQASM 2 program is invalid, or otherwise cannot be converted
        to Qiskit format.

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
    return parse.from_bytecode(core.bytecode_from_file(_normalize_path(filename), include_path))
