import contextlib
from prestring import INDENT, UNINDENT
from handofcats.actions._codeobject import Module as _Module
from handofcats.actions.commandline import _FakeModule

"""
with block() as p:
    if p(True):
        print("hoi")
"""


class Fail(Exception):
    pass


class FakeModule(_FakeModule):
    def stmt(self, *args):
        pass

    def append(self, *args):
        pass

    def check(self, cond):
        if not cond:
            raise Fail(cond)
        return True


class Module(_Module):
    def check(self, cond):
        return True


@contextlib.contextmanager
def if_(m: Module, cond):
    try:
        m.stmt("if {}", cond)
        m.append(INDENT)
        yield m.check(cond)
    except Fail:
        yield False
    finally:
        m.append(UNINDENT)


def run(m, cond):
    a = m.let("a", True)
    b = m.let("b", cond)
    with if_(m, m.is_not(a, b)) as p:
        if p:
            print_ = m.symbol(print)
            m.stmt(print_("ok"))
    m.stmt("# end")
    return m


print(run(Module(), True))
print(run(Module(), False))
print("----------------------------------------")
print(run(FakeModule(), True))
print(run(FakeModule(), False))
