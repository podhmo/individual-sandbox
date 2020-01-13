import contextlib
from prestring.utils import LazyFormat
from _codeobject import Module as _Module
from _codeobject import Symbol


class FakeModule:
    @contextlib.contextmanager
    def if_(self, x):
        if not x:
            raise Fail()
        yield None

    def else_(self, x):
        return self.if_(self.not_(x))

    def not_(self, x):
        return not x

    def stmt(self, x):
        pass

    def symbol(self, x):
        return x


class Module(_Module):
    def else_(self, x):
        return self.if_(self.not_(x))

    def not_(self, x):
        return LazyFormat("not {}", x)

    def symbol(self, x):
        if hasattr(x, "__name__"):
            return Symbol(x.__name__)
        return Symbol(x)


class Fail(Exception):
    pass


def run(m):
    print_ = m.symbol(print)

    with m.if_(True):
        m.stmt(print_("ok"))
    print_("----------------------------------------")

    with contextlib.suppress(Fail):
        with m.if_(False):
            m.stmt(print_("ng"))
    m.stmt(print_("ok"))

    print_("----------------------------------------")

    with contextlib.suppress(Fail):
        with m.if_(False):
            m.stmt(print_("ng"))
    with contextlib.suppress(Fail):
        with m.else_(False):
            m.stmt(print_("ok"))

    print_("----------------------------------------")
    with contextlib.suppress(Fail):
        with m.if_(True):
            m.stmt(print_("ok"))
    with contextlib.suppress(Fail):
        with m.else_(True):
            m.stmt(print_("ng"))


m = FakeModule()
run(m)
print("========================================")
m = Module()
run(m)
print(m)
