from distutils.core import setup

setup(
    name="myapp",
    version="0.0.0",
    packages=["foo"],
    entry_points="""
[console_scripts]
myapp2 = foo.__main__:main
""",
)
