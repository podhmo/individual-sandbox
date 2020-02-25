import foo
import importlib

x = foo.FOO
print(foo.FOO, x, foo.FOO == x, foo.FOO is x)

importlib.reload(foo)
print(foo.FOO, x, foo.FOO == x, foo.FOO is x)
