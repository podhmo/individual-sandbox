import typing as t
from prestring import output
from io import StringIO
from prestring.python import Module

with output.output("", use_console=True, verbose=True, opener=StringIO) as fs:
    with fs.open("foo.py", "w", opener=Module) as m:
        m.stmt("print({!r})", "foo")
    with fs.open("VERSION", "w", opener=StringIO) as io:
        io.write("0.0.0")
    with fs.open("xxx", "w") as x:  # type: t.IO[str]
        x.write("xxx")
