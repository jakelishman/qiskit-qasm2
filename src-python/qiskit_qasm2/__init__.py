from pathlib import Path as _Path

with open(_Path(__file__).parent / "VERSION", "r", encoding="utf-8") as _version_file:
    __version__ = _version_file.read().strip()

from . import core, parse
from .core import QASM2ParseError


def loads(string):
    return parse.from_bytecode(core.bytecode_from_string(string))


def load(path):
    return parse.from_bytecode(core.bytecode_from_file(path))
