from setuptools import setup, find_packages

install_requires = [
]

setup(
    name='foo',
    version='0.0.1',
    description='my foo package',
    long_description='my foo package long description',
    packages=find_packages(exclude=["foo.tests"]),
    install_requires=install_requires,
    test_suite="foo.tests",
)
