import inspect
from yaml.constructor import Constructor
from prestring.python import Module


m = Module()
m.from_("yaml.constructor", "Constructor")
m.sep()
with m.class_("WrappedConstructor", "Constructor"):
    with m.def_("wrap", "self", "path", "name", "node", "r"):
        with m.if_("r is None"):
            m.stmt("return r")
        m.stmt('# print("@", id(r), repr(r))')
        m.stmt("mem[id(r)] = node")
        m.stmt("return r")

    seen = set()
    for cls in Constructor.mro():
        for name, attr in cls.__dict__.items():
            if name in seen:
                continue
            seen.add(name)
            if name.startswith("construct_") and callable(attr):
                sigs = inspect.signature(attr)
                m.stmt("def {}{}:", name, sigs)
                with m.scope():
                    args = []
                    for v in sigs.parameters.values():
                        if v.name == "self":
                            continue
                        if v.default is inspect._empty:
                            args.append(str(v))
                        else:
                            args.append(f"{v.name}={v.name}")
                    nodeval = "node" if "node" in sigs.parameters else "None"
                    m.stmt("path.append(node)")
                    m.stmt(f"r = super().{name}({', '.join(args)})")
                    m.stmt(f"self.wrap(path, {name!r}, {nodeval}, r)")
                    m.stmt("path.pop()")
                    m.return_("r")
                m.sep()
print(m)
