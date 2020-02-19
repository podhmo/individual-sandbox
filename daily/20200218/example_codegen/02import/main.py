from prestring.output import output
from prestring.python import Module
import hello


with output("", opener=Module, use_console=True, verbose=True) as fs:
    with fs.open(hello.__name__, "w") as m:
        hello.hello(m)
