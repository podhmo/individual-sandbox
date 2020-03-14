from prestring.python import PythonModule
from prestring.codeobject import CodeObjectModuleMixin
from q import q


def predicate(d):
    name = d["name"]
    return name.startswith(":")


print(predicate({"name": ":foo:"}))


class Module(PythonModule, CodeObjectModuleMixin):
    pass


class Builder:
    def __init__(self, q, *, name: str = "_") -> None:
        self.var_names = set()
        self.name = name
        self.q = q

    def build_code(self):
        m = Module()
        d = m.symbol("d")
        q = self.q
        with m.def_(self.name, d):
            env = {}
            for name in self.var_names:
                env[name] = m.let(name, d.get(name))  # TODO: required/unrequired
            m.return_(q)
        return m


# from q import QJSBuilder
# from functools import partial
# q = partial(q, builder=QJSBuilder())

print(q("x"), q("x").And("y"), q("x").Lt(q("y").Not()))
print(q("x").startswith("y"), q("x").foo.bar(q("z"), "xx").boo())
print(q("x").IsNot("y"), q("x").In(q("y")))
print(q("x").Add("y").neg().add(q("int")(True)))
print(q("ob").x.Mult(q("ob").y))

b = Builder(q("name").startswith("o"))
b.var_names.add("name")
print(b.build_code())
