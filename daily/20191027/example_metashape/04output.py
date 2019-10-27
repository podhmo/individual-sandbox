from output import SeparatedOutput
from prestring.python import Module

# context manager


o = SeparatedOutput("./04output", prefix="gen_", module_factory=Module)
with o:
    with o.open("x.py", "w") as f:
        with f.m.def_("hello()"):
            f.m.stmt("print('x')")

    with o.open("y.py", "w") as f:
        with f.m.def_("hello()"):
            f.m.stmt("print('y')")
