from prestring.python import PythonModule
from prestring.codeobject import CodeObjectModuleMixin


class Module(PythonModule, CodeObjectModuleMixin):
    pass


print(Module.mro())
m = Module()
m.stmt("foo")
print(m)
