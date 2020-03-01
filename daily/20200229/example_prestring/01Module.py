from prestring.python import PythonModule
from prestring.codeobject import CodeObjectModuleMixin



class Module(CodeObjectModuleMixin, PythonModule):
    pass


print(Module.mro())
m = Module()
m.stmt("foo")
print(m)
