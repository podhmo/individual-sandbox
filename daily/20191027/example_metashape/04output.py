import logging
from output import FS, Option, PrestringResolver
from prestring.python import Module

logging.basicConfig(level=logging.DEBUG)

option = Option(prefix="gen_", root="./04output")
r = PrestringResolver(module_factory=Module)

with FS(option) as fs:
    with fs.file(r.resolve("x.py")) as m:
        with m.def_("hello()"):
            m.stmt("print('x')")

    with fs.file(r.resolve("y.py")) as m:
        with m.def_("hello()"):
            m.stmt("print('y')")
