from prestring.python import Module
import inspect


def parse(cls, *, registry=None):
    registry = registry or {}

    if cls in registry:
        return registry

    d = attrs = {}
    d = registry[cls] = {"__mro__": cls.mro(), "attrs": attrs}

    for super_class in d["__mro__"][1:]:
        if super_class not in registry:
            registry = parse(super_class, registry=registry)

    for name, attr in cls.__dict__.items():
        if name.startswith("__") and name.endswith("__"):
            continue

        mismatch = False
        for c in d["__mro__"][1:]:
            if hasattr(c, name):
                mismatch = True
                break
        if mismatch:
            continue

        kind = get_kind(attr)
        if kind not in attrs:
            attrs[kind] = {}
        attrs[kind][name] = attr
    return registry


def get_kind(attr):
    if isinstance(attr, staticmethod):
        return "static_method"
    elif isinstance(attr, classmethod):
        return "class_method"
    elif isinstance(attr, property):
        return "property"
    elif inspect.isroutine(attr):  # xxx
        return "method"
    else:
        return "data"


def emit(registry, m=None, ignore_modules=("builtins", "collections")):
    m = m or Module()
    for cls, d in registry.items():
        if cls.__module__ in ignore_modules:
            continue
        emit_class(cls, d, m=m)
    # todo: sort
    return m


def emit_class(cls, d, m):
    with m.class_(cls.__name__, " <- ".join([c.__name__ for c in d["__mro__"]])):
        if cls.__doc__:
            m.stmt('"""{}"""', cls.__doc__.rstrip("\n"))

        attrs = d["attrs"]
        if len(attrs) == 0:
            m.stmt("pass")
            return

        if "class_method" in attrs:
            for name, attr in attrs["class_method"].items():
                argspec = inspect.getfullargspec(attr.__func__)
                m.stmt("@classmethod({}({}))", name, ", ".join(argspec.args))

        if "method" in attrs:
            for name, attr in attrs["method"].items():
                try:
                    m.stmt("{}{}", name, str(inspect.signature(attr)))
                except Exception as e:
                    m.stmt("#", name, "##", str(e))
