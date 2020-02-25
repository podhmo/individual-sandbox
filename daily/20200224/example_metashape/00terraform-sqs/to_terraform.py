import dataclasses
from metashape.runtime import get_walker, get_name
from prestring.text import Module


def to_val(v):
    import json

    return json.dumps(v)


def scan(d):
    resources = {}
    for name, val in tuple(d.items()):
        if hasattr(val, dataclasses._FIELDS):
            resources[id(val)] = name  # xxx:
    return resources


def emit(targets, *, resources):
    m = Module(indent="  ")
    w = get_walker([])

    for i, cls in enumerate(targets):
        name = resources[id(cls)]  # xxx:

        m.stmt("resource {} {} {{", to_val(get_name(cls)), to_val(name))
        with m.scope():
            for name, info, metadata in w.for_type(cls).walk():
                value = metadata["default"]
                if value is None:
                    continue
                m.stmt("{} = {}", name, to_val(value))
        m.stmt("}")
        if i < len(targets) - 1:
            m.sep()
    return m


def emit2(targets, *, resources):
    m = Module(indent="  ")
    w = get_walker([])

    m.stmt('include classpath("application.conf")')
    m.stmt("queues {")
    with m.scope():
        for i, cls in enumerate(targets):
            name = resources[id(cls)]  # xxx:

            m.stmt("{} {{", name)
            with m.scope():
                for name, info, metadata in w.for_type(cls).walk():
                    value = metadata["default"]
                    if value is None:
                        continue
                    if isinstance(value, int):
                        m.stmt("{} = {} seconds", name, value)
                    else:
                        m.stmt("{} = {}", name, to_val(value))
            m.stmt("}")
            if i < len(targets) - 1:
                m.sep()
    m.stmt("}")
    return m
