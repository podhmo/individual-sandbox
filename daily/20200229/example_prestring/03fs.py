from prestring import output
from prestring.python import Module

with output.output("", use_console=True, verbose=True, opener=Module) as fs:
    with fs.open("foo.py", "w") as m:
        m.stmt("print({!r})", "foo")
