from setuptools import setup, find_packages

README = ""

install_requires = []

tests_require = []

testing_extras = tests_require + []

setup(
    name="foo",
    version="0.0",
    description="-",
    package_data={"foo": ["py.typed"]},
    packages=find_packages(exclude=["foo.tests"]),
    include_package_data=True,
    zip_safe=False,
    install_requires=install_requires,
    tests_require=tests_require,
    test_suite="foo.tests",
)
