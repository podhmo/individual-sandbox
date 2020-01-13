from _codeobject import Module
from _fake import _FakeModule


# transform f -> mf
def f(x):
    print("hello", x)


def mf(m, x):
    print_ = m.symbol(print)
    m.stmt(print_("hello", x))


print("original")
f("world")

print("\n----------------------------------------\n")

print("fake")
m = _FakeModule()
mf(m, "world")

print("\n----------------------------------------\n")


print("codeobject")
m = Module()
mf(m, "world")
print(m)
