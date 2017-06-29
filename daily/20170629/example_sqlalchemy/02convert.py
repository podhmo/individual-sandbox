# -*- coding:utf-8 -*-
from dictknife import loading
from prestring import Module
import contextlib
import logging
logger = logging.getLogger(__name__)


def titleize(name):
    if not name:
        return name
    return name[0].upper() + name[1:]


def singular(name):
    if name.endswith("s"):
        return titleize(name[:-1])
    return titleize(name)


class Array:
    def __init__(self, t):
        self.t = t

    def __str__(self):
        return "[{}]".format(self.t)


class GraphQLModule(Module):
    @contextlib.contextmanager
    def type_(self, name):
        self.stmt("type {}", name)
        with self.scope():
            yield

    def field(self, name, typ, nullable=True):
        if nullable:
            self.stmt("{}: {}", name, typ)
        else:
            self.stmt("{}: {}!", name, typ)


def emit(m, d):
    for name, fields in d.items():
        with m.type_(singular(name)):
            for k, v in fields.items():
                if "type" in v:
                    m.field(k, v["type"], nullable=v.get("nullable", True))
                else:
                    if v["uselist"]:
                        m.field(k, Array(singular(v["table"])), nullable=v.get("nullable", True))
                    else:
                        m.field(k, singular(v["table"]), nullable=v.get("nullable", True))
        m.stmt("")


def main(src):
    d = loading.loadfile(src)
    m = GraphQLModule()
    emit(m, d)
    print(m)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    args = parser.parse_args()
    main(args.src)
