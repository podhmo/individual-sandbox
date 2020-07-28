import typing as t
from prestring.python.codeobject import Module
from egoist.go.walker import get_walker
from egoist.generators.structkit._emit import emit_struct


def emit(classes: t.List[t.Type[t.Any]]) -> Module:
    w = get_walker(classes)
    for item in w.walk():
        emit_struct(w, item)
    return w.m
