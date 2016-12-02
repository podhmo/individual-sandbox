# -*- coding:utf-8 -*-
# see: https://mholt.github.io/json-to-go/
# original: https://github.com/mholt/json-to-go/blob/master/json-to-go.js
import sys  # NOQA
import re
import json
from collections import defaultdict, deque
from prestring.go import GoModule
from prestring import LazyFormat, PreString, NameStore, Module
from prestring.go import goname as to_goname


def json_to_go(json_string, name, m=None, rx=re.compile("\.0", re.M)):
    data = json.loads(rx.sub(".1", json_string))
    s = detect_struct_info(data, name)
    return emit_code(s, name, m=m)


def resolve_type(val, time_rx=re.compile("\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(\+\d\d:\d\d|Z)")):
    if val is None:
        return "interface{}"
    if isinstance(val, str):
        if time_rx.match(val):
            return "time.Time"
        elif "://" in val:
            return "github.com/go-openapi/strfmt.Uri"
        else:
            return "string"
    elif isinstance(val, int):
        if val > -2147483648 and val < 2147483647:
            return "int"
        else:
            return "int64"
    elif isinstance(val, float):
        return "float64"
    elif isinstance(val, bool):
        return "bool"
    elif hasattr(val, "keys"):
        return "struct"
    elif isinstance(val, (list, tuple)):
        return "slice"
    else:
        raise ValueError("unsupported for {!r}".format(val))


def select_better_type(*types):
    s = {t for t in types if t is not None}
    if "float64" in s:
        return "float64"
    elif "int64" in s:
        return "int64"
    else:
        return s.pop()


def detect_struct_info(d, name):
    def _detect_struct_info(d, s, name):
        if hasattr(d, "keys"):
            s["type"] = "struct"
            s["jsonname"] = name
            s["freq"] += 1
            for k, v in d.items():
                goname = to_goname(k)
                _detect_struct_info(v, s["children"][goname], k)
        elif isinstance(d, (list, tuple)):
            s["type2"] = "slice"
            for x in d:
                _detect_struct_info(x, s, name)  # xxx
        else:
            typ = resolve_type(d)
            s["jsonname"] = name
            s["freq"] += 1
            s["type"] = select_better_type(s["type"], typ)
            s["example"] = d

    def make_struct_info():
        return {"freq": 0, "type": None, "children": defaultdict(make_struct_info)}
    s = defaultdict(make_struct_info)
    goname = to_goname(name)
    _detect_struct_info(d, s[goname], goname)
    return s[goname]


def to_type_struct_info(sinfo):
    if sinfo.get("type2") == "slice":
        return "[]" + sinfo["type"]
    else:
        return sinfo["type"]


def is_omitempty_struct_info(subinfo, sinfo):
    return subinfo["freq"] < sinfo["freq"]


class CommentWriter(object):
    def __init__(self, m, name, sinfo):
        m.stmt("/* structure")
        cm = Module()
        m.stmt(cm)
        cm.stmt(name)
        self.cm_map = {sinfo["jsonname"]: cm}
        m.stmt("*/")

    def write(self, name, sinfo, parent=None):
        if parent is None:
            return
        cm = self.cm_map[parent["jsonname"]]
        with cm.scope():
            cm.stmt(name)
            self.cm_map[sinfo["jsonname"]] = cm.submodule(newline=False)


class StructWriter(object):
    def __init__(self, m, cw, im, score_map=None):
        self.m = m
        self.cw = cw
        self.im = im
        self.cont = deque()
        self.name_store = NameStore()
        self.defined = set()
        self.typename_map = defaultdict(lambda: PreString(""))
        self.score_map = score_map or {}

    def write_field(self, name, sinfo, parent=None):
        typ = sinfo.get("type")

        if typ == "struct":
            self.cw.write(name, sinfo, parent=parent)
            signature = make_signature(sinfo)
            self.cont.append((name, sinfo, signature))
            typename = self.typename_map[signature]
            self.m.stmt(LazyFormat('{} {}', name, typename))
        else:
            if "." in typ:
                self.im.import_(typ.rsplit(".", 1)[0])
            typ = to_type_struct_info(sinfo)
            if "/" in typ:
                typ = typ.rsplit("/", 1)[-1]
            self.m.stmt('{} {}'.format(name, typ))

        # append tag
        if "example" in sinfo:
            example = ' example:"{}"'.format(sinfo["example"])
        else:
            example = ''
        if is_omitempty_struct_info(sinfo, parent):
            omitempty = '.omitempty'
        else:
            omitempty = ''
        self.m.insert_after(' `json:"{name}{omitempty}"{example}`'.format(name=sinfo["jsonname"], omitempty=omitempty, example=example))

    def write_struct(self, name, signature, sinfo, parent=None):
        if signature in self.defined:
            return
        self.defined.add(signature)

        with self.m.type_(name, to_type_struct_info(sinfo)):
            for name, subinfo in sorted(sinfo["children"].items()):
                self.write_field(name, subinfo, parent=sinfo)

    def write(self, name, sinfo):
        signature = make_signature(sinfo)
        self.write_struct(name, signature, sinfo)
        while self.cont:
            name, sinfo, signature = self.cont.popleft()
            self.name_store[signature] = name
            self.typename_map[signature].append(self.name_store[signature])
            self.write_struct(self.name_store[signature], signature, sinfo)

        for typename in self.typename_map.values():
            name = list(reversed(sorted(typename.body, key=lambda x: self.score_map.get(x, 0))))[0]
            typename.clear()
            typename.append(name)


def make_signature(sinfo):
    return tuple([(k, v["type"], v.get("type2")) for k, v in sorted(sinfo["children"].items())])


def emit_code(sinfo, name, m=None, score_map={"Parent": -1, "": -1}):
    m = m or GoModule()
    with m.import_group() as im:
        cw = CommentWriter(m, name, sinfo)
    writer = StructWriter(m, cw, im, score_map=score_map)
    writer.write(name, sinfo)
    im.clear_ifempty()
    return m


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--package", type=str, default="autogen")
    parser.add_argument("--name", type=str, default="AutoGenerated")
    parser.add_argument("src", type=argparse.FileType('r'))
    args = parser.parse_args()

    m = GoModule()
    m.package(args.package)
    print(json_to_go(args.src.read(), args.name, m))
