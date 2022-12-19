__version__ = "0.1.0"

from . import core, parse

from .core import QASM2ParseError

def loads(string):
    return parse.from_bytecode(core.bytecode_from_string(string))


def load(path):
    return parse.from_bytecode(core.bytecode_from_file(path))
