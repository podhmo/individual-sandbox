# -*- coding:utf-8 -*-
# see: https://mholt.github.io/json-to-go/
# original: https://github.com/mholt/json-to-go/blob/master/json-to-go.js
import sys  # NOQA
import re
import json
from collections import defaultdict, deque
from prestring.go import GoModule
from prestring import LazyFormat, PreString
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


def emit_code(sinfo, name, m=None, score_map={"Parent": -1, "": -1}):
    def make_signature(sinfo):
        return tuple([(k, v["type"], v.get("type2")) for k, v in sorted(sinfo["children"].items())])

    def _emit_struct(sinfo, name, m, parent=None):
        with m.type_(name, to_type_struct_info(sinfo)):
            for name, subinfo in sorted(sinfo["children"].items()):
                _emit_code(subinfo, name, m, parent=sinfo)

    def _emit_code(sinfo, name, m, parent=None):
        typ = sinfo.get("type")
        if parent is not None and typ == "struct":
            cm = m.cm_map[parent["jsonname"]]
            with cm.scope():
                cm.stmt(name)
                m.cm_map[sinfo["jsonname"]] = cm.submodule(newline=False)

        if typ == "struct":
            signature = make_signature(sinfo)
            cont.append((name, sinfo, signature))
            m.stmt(LazyFormat('{} {}', name, typename_map[signature]))
        else:
            m.stmt('{} {}'.format(name, to_type_struct_info(sinfo)))

        # append tag
        if is_omitempty_struct_info(sinfo, parent):
            m.insert_after('  `json:"{},omitempty"`'.format(sinfo["jsonname"]))
        else:
            m.insert_after('  `json:"{}"`'.format(sinfo["jsonname"]))

    m = m or GoModule()
    if sinfo.get("type") != "struct":
        raise ValueError("hmm")

    typename_map = defaultdict(lambda: PreString(""))
    defined = set()
    cont = deque([(name, sinfo, make_signature(sinfo))])
    name_store = NameStore()

    m.stmt("/* structure")
    with m.scope():
        m.stmt(name)
        cm = m.submodule(newline=False)
        m.cm_map = {sinfo["jsonname"]: cm}
    m.stmt("*/")

    while cont:
        name, sinfo, signature = cont.popleft()
        lazy_type_name = typename_map[signature]
        name_store[signature] = name
        lazy_type_name.append(name_store[signature])
        if signature in defined:
            continue
        defined.add(signature)
        _emit_struct(sinfo, name_store[signature], m)

    for typename in typename_map.values():
        typename.body = list(reversed(sorted(typename.body, key=lambda x: score_map.get(x, 0))))[0]
    return m


class NameStore(object):
    def __init__(self):
        self.c = defaultdict(int)
        self.value_map = {}  # (src_type, dst_type) => (name, i)

    def __contains__(self, value):
        return value in self.value_map

    def __setitem__(self, value, name):
        if value not in self.value_map:
            self.value_map[value] = self.get_name(value, name)
            self.c[name] += 1

    def __getitem__(self, value):
        return self.value_map[value]

    def get_name(self, value, name):
        try:
            return self[value]
        except KeyError:
            i = self.c[name]
            return self.new_name(name, i)

    def new_name(self, name, i):
        if i == 0:
            return name
        else:
            return "{}Dup{}".format(name, i)


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