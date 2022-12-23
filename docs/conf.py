import qiskit_qasm2

project = 'Qiskit OpenQASM 2 Tools'
copyright = '2022, Jake Lishman'
author = 'Jake Lishman'
version = qiskit_qasm2.__version__

extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.intersphinx",
]
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# Document the docstring for the class and the __init__ method together.
autoclass_content = "both"

html_theme = 'alabaster'

intersphinx_mapping = {
    "qiskit-terra": ("https://qiskit.org/documentation", None),
}
