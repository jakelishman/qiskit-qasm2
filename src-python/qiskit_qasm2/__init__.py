__version__ = "0.1.0"

from . import core as _core
from . import parse as _parse


def loads(string):
    return _parse.from_bytecode(_core.bytecode_from_string(string))


def load(path):
    return _parse.from_bytecode(_core.bytecode_from_file(path))
