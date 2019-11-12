from prestring.python import Module


def gen() -> Module:
    m = Module()
    with m.def_("foo", "x", "y", return_type="int"):
        m.stmt("pass")

    with m.def_("bar", "x", return_type="int"):
        m.stmt("pass")
    return m


print(gen())
