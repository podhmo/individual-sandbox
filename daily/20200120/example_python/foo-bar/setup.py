from setuptools import setup, find_packages

setup(
    name="foo-bar",
    version="0.0.0",
    long_description="foo-bar",
    packages=find_packages(where="foo"),
)
