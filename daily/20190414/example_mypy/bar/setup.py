from setuptools import setup, find_packages

README = ""

install_requires = []

tests_require = []

testing_extras = tests_require + []

setup(
    name="bar",
    version="0.0",
    description="-",
    package_data={"bar": ["py.typed"]},
    packages=find_packages(exclude=["bar.tests"]),
    include_package_data=True,
    zip_safe=False,
    install_requires=install_requires,
    tests_require=tests_require,
    test_suite="bar.tests",
)
