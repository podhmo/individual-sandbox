from prestring.python import Module


def emit_foo(m: Module, name: str, *, sep: str):
    with m.def_(name, "message: str"):
        m.return_(f"f'foo{sep}{{message}}'")
    return m


m = Module()
m = emit_foo(m, "do_foo", sep=":")

with m.for_("i", "range(5)"):
    m.stmt("do_foo(str(i))")

print(m)
