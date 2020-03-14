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


print(q("x"), q("x").and_("y"), q("x").lt(q("y").not_()))
print(q("x").startswith("y"), q("x").foo.bar(q("z"), "xx").boo())
print(q("x").is_not("y"), q("x").contains(q("y")))
print(q("x").add("y").neg().add(q("int")(True)))

b = Builder(q("name").startswith("o"))
b.var_names.add("name")
print(b.build_code())
