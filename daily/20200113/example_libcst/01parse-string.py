import libcst
import inspect
import hello
from libcst.tool import dump

code = inspect.getsource(hello)
t = libcst.parse_module(code)


print(type(t))
print(t)
print("----------------------------------------")
print(dump(t))
