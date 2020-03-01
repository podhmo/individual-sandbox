import typing as t
from prestring import output
from prestring.python import Module

with output.output("", use_console=True, verbose=True) as fs:
    with fs.open("foo.py", "w", opener=Module) as m:  # type: Module
        m.stmt("print({!r})", "foo")
    with fs.open("VERSION", "w") as io:  # type: t.IO[str]
        io.write("0.0.0")
