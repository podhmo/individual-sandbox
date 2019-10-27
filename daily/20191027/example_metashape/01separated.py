from prestring.output import SeparatedOutput
from prestring.python import Module

o = SeparatedOutput("./01separated", prefix="gen_", module_factory=Module)

f = o.new_file("x.py")
o.files[f.name] = f
with f.m.def_("hello()"):
    f.m.stmt("print('x')")

f = o.new_file("y.py")
o.files[f.name] = f
with f.m.def_("hello()"):
    f.m.stmt("print('y')")

o.output()
