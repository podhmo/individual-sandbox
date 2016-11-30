# -*- coding:utf-8 -*-
# see: https://mholt.github.io/json-to-go/
# original: https://github.com/mholt/json-to-go/blob/master/json-to-go.js
import re
import json
from functools import partial
from collections import namedtuple
from collections import defaultdict
from collections.abc import Mapping, Sequence
from prestring.go import GoModule


def json_to_go(json_string, name, m=None, rx=re.compile("\.0", re.M)):
    data = json.loads(rx.sub(".1", json_string))
    s = Detector().detect(data, name)
    return Emitter().emit(s, name, m=m)


Type = namedtuple("Type", "name module")
Primitive = partial(Type, module=None)

Int = Primitive("int")
Int64 = Primitive("int64")
Float64 = Primitive("float64")
String = Primitive("string")
Bool = Primitive("bool")

Any = Primitive("interface{}")
Struct = Primitive("struct")
Slice = Primitive("slice")

Time = Type("Time", module="time")


class TypeResolver:
    def resolve(self, val):
        if val is None:
            return Any
        if isinstance(val, str):
            return self._resolve_string(val)
        elif isinstance(val, int):
            return self._resolve_int(val)
        elif isinstance(val, float):
            return self._resolve_float(val)
        elif isinstance(val, bool):
            return self._resolve_bool(val)
        elif hasattr(val, Mapping):
            return self._resolve_dict(val)
        elif hasattr(val, Sequence):
            return self._resolve_array(val)
        else:
            return Any

    def _resolve_string(self, val, time_rx=re.compile("\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(\+\d\d:\d\d|Z)")):
        if time_rx.match(val):
            return Time
        else:
            return String

    def _resolve_int(self, val):
        if val > -2147483648 and val < 2147483647:
            return Int
        else:
            return Int64

    def _resolve_float(self, val):
        return Float64

    def _resolve_bool(self, val):
        return Bool

    def _resolve_dict(self, val):
        return Struct

    def _resolve_array(self, val):
        return Slice

    def select(self, *types):
        s = {t for t in types if t is not None}
        if Float64 in s:
            return Float64
        elif Int64 in s:
            return Int64
        else:
            return s.pop()


class Detector:
    def __init__(self):
        def make_state():
            return {"freq": 0, "type": None, "children": defaultdict(make_state)}

        self.state = defaultdict(make_state)
        self.type_resolver = TypeResolver()
        self.formatter = NameFormatter()

    def make_state(self, name, typ):
        return {"name": name, "freq": 0, "type": typ, "children": {}}

    def detect(self, d, name):
        goname = self.formatter.format(name)
        self._detect(d, self.state[goname], name)
        return self.state[goname]

    def _detect(self, d, s, name):
        if hasattr(d, "keys"):
            return self._detect_dict(d, s, name)
        elif isinstance(d, (list, tuple)):
            return self._detect_list(d, s, name)
        else:
            return self._detect_value(d, s, name)

    def _detect_dict(self, d, s, name):
        s["type"] = Struct
        s["jsonname"] = name
        s["freq"] += 1
        for k, v in d.items():
            goname = self.formatter.format(k)
            self._detect(v, s["children"][goname], k)

    def _detect_list(self, xs, s, name):
        s["type2"] = Slice
        for x in xs:
            self._detect(x, s, name)  # xxx

    def _detect_value(self, v, s, name):
        typ = self.type_resolver.resolve(v)
        s["jsonname"] = name
        s["freq"] += 1
        s["type"] = self.type_resolver.select(s["type"], typ)


class NameFormatter:
    def __init__(self):
        self.numbers = {
            '0': "Zero_", '1': "One_", '2': "Two_", '3': "Three_",
            '4': "Four_", '5': "Five_", '6': "Six_", '7': "Seven_",
            '8': "Eight_", '9': "Nine_"
        }

    def format(self, s,
               num_rx=re.compile("\d{2,}"),
               exclude_rx=re.compile("[^a-z0-9]", re.IGNORECASE | re.MULTILINE)):
        if not s:
            return ""
        elif num_rx.match(s):
            return self.format("Num" + s)
        elif s[0] in self.numbers:
            return self.format(self.numbers[s[0]] + s[1:])
        else:
            return exclude_rx.sub("", self.proper_acronym(s))

    def titlize(self, s):
        if not s:
            return s
        return s[0].upper() + s[1:]

    def proper_acronym(self, s,
                       rx=re.compile("(?P<sep>^|[^a-zA-Z])(?P<frag>[a-z]+)", re.M),
                       rx2=re.compile("(?P<sep>[A-Z])(?P<frag>[a-z]+)", re.M)):
        # https://github.com/golang/lint/blob/39d15d55e9777df34cdffde4f406ab27fd2e60c0/lint.go#L695-L731
        common_initialisms = [
            "API", "ASCII", "CPU", "CSS", "DNS", "EOF", "GUID", "HTML", "HTTP",
            "HTTPS", "ID", "IP", "JSON", "LHS", "QPS", "RAM", "RHS", "RPC", "SLA",
            "SMTP", "SSH", "TCP", "TLS", "TTL", "UDP", "UI", "UID", "UUID", "URI",
            "URL", "UTF8", "VM", "XML", "XSRF", "XSS"
        ]

        def repl(m):
            d = m.groupdict()
            if d["frag"].upper() in common_initialisms:
                return d["sep"] + d["frag"].upper()
            else:
                return d["sep"] + self.titlize(d["frag"])

        def repl2(m):
            d = m.groupdict()
            merged = d["sep"] + d["frag"]
            if merged.upper() in common_initialisms:
                return merged.upper()
            else:
                return merged

        return rx2.sub(repl2, rx.sub(repl, s))


class ValueFormatter:  # TODO: rename
    def to_type(self, state):
        if state["type"].module is None:
            value = state["type"].name
        else:
            value = "{}.{}".format(state["type"].module, state["type"].name)
        if state.get("type2") == Slice:
            return "[]" + value
        else:
            return value

    def to_jsonname(self, state):
        return state["jsonname"]

    def is_omitempty(self, substate, state):
        return substate["freq"] < state["freq"]


class Emitter:
    def __init__(self):
        self.value_formatter = ValueFormatter()

    def emit(self, state, name, m=None):
        m = m or GoModule()
        if state.get("type") != Struct:
            raise ValueError("hmm")

        with m.type_(name, self.value_formatter.to_type(state)):
            for name, substate in sorted(state["children"].items()):
                self._emit(substate, name, m, parent=state)
        return m

    def _emit(self, state, name, m, parent=None):
        typ = state.get("type")
        if typ == Struct:
            return self._emit_struct(state, name, m, parent=parent)
        else:
            return self._emit_field(state, name, m, parent=parent)

    def _emit_struct(self, state, name, m, parent=None):
        with m.block("{} struct".format(name)):
            for name, substate in sorted(state["children"].items()):
                self._emit(substate, name, m, parent=state)
        jsonname = self.value_formatter.to_jsonname(state)
        if self.value_formatter.is_omitempty(state, parent):
            m.insert_after('  `json:"{},omitempty"`'.format(jsonname))
        else:
            m.insert_after('  `json:"{}"`'.format(jsonname))

    def _emit_field(self, state, name, m, parent=None):
        typ = self.value_formatter.to_type(state)
        jsonname = self.value_formatter.to_jsonname(state)
        if self.value_formatter.is_omitempty(state, parent):
            m.stmt('{} {}  `json:"{},omitempty"`'.format(name, typ, jsonname))
        else:
            m.stmt('{} {}  `json:"{}"`'.format(name, typ, jsonname))


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
