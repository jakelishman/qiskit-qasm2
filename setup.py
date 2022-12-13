from setuptools import setup
from setuptools_rust import RustExtension, Binding

setup(
    rust_extensions=[RustExtension("qiskit_qasm2.core", "Cargo.toml", binding=Binding.PyO3)],
)
