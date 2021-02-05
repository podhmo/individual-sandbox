from setuptools import setup, find_packages

setup(
    name="myapp",
    version="0.0.0",
    packages=find_packages(exclude=["foo.tests"]),
    entry_points={"console_scripts": {"myapp2": "foo.__main__:main"}},
)
