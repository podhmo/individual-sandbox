import copy
from dictknife import loading
from collections import OrderedDict
from prestring import NameStore

"""
- conflict check
- deduplication
- annotation schema name
"""


def resolve_type(val):
    if isinstance(val, str):
        return "string"
    elif isinstance(val, bool):
        return "boolean"
    elif isinstance(val, int):
        return "integer"
    elif isinstance(val, float):
        return "number"
    elif hasattr(val, "keys"):
        return "object"
    elif isinstance(val, (list, tuple)):
        return "array"
    else:
        raise ValueError("unsupported for {!r}".format(val))


class NameResolver:
    def __init__(self):
        self.ns = NameStore()


def make_signature(info):
    if info.get("type2") == "array":
        return frozenset(["*a*", *[(k, v["type"], v.get("type2")) for k, v in info["children"].items()]])
    elif info["type"] == "object":
        return frozenset((k, v["type"], v.get("type2")) for k, v in info["children"].items())
    else:
        return frozenset((info["type"], info.get("type2")))


class Detector:
    def make_info(self):
        return {"freq": 0, "freq2": 0, "type": "any", "children": OrderedDict(), "values": []}

    def detect(self, d, name):
        s = OrderedDict()
        s[name] = self.make_info()
        path = [name]
        self._detect(d, s[name], name, path=path)
        return s[name]

    def _detect(self, d, s, name, path):
        if hasattr(d, "keys"):
            s["type"] = "object"
            s["name"] = name
            s["freq"] += 1
            for k, v in d.items():
                if k not in s["children"]:
                    s["children"][k] = self.make_info()
                path.append(k)
                self._detect(v, s["children"][k], k, path=path)
                path.pop()
            s["ref"] = "#/{}".format("/".join(path))
        elif isinstance(d, (list, tuple)):
            s["type2"] = "array"
            s["freq2"] += 1
            for i, x in enumerate(d):
                path.append(str(i))
                self._detect(x, s, name, path=path)  # xxx
                path.pop()
            s["ref"] = "#/{}".format("/".join(path))
        else:
            if d is None:
                s["type2"] = "null"
            else:
                typ = resolve_type(d)
                s["name"] = name
                s["freq"] += 1
                s["type"] = typ
                s["values"].append(d)
            s["ref"] = "#/{}".format("/".join(path))


class Emitter:
    def __init__(self, annotations):
        self.doc = OrderedDict(definitions=OrderedDict())
        self.definitions = self.doc["definitions"]
        self.ns = NameStore()
        self.annotations = annotations  # Dict[string, Dict]

    def resolve_name(self, info):
        name = self.annotations.get(info["ref"]) or info["name"]
        signature = info["signature"]
        self.ns[signature] = name
        return self.ns[signature]

    def make_schema(self, info):
        info["signature"] = make_signature(info)  # xxx:
        if info.get("type2") == "array":
            return self.make_array_schema(info)
        elif info.get("type") == "object":
            return self.make_object_schema(info)
        else:
            return self.make_primitive_schema(info)

    def make_array_schema(self, info):
        item_info = copy.deepcopy(info)
        item_info.pop("type2")
        item_info["name"] = "{}Item".format(item_info["name"])

        d = OrderedDict(type="array")
        d["items"] = self.make_schema(item_info)
        schema_name = self.resolve_name(info)
        self.definitions[schema_name] = d
        return {"$ref": "#/definitions/{name}".format(name=schema_name)}

    def make_object_schema(self, info):
        d = OrderedDict(type="object")
        d["properties"] = OrderedDict()
        props = d["properties"]
        for name, value in info["children"].items():
            props[name] = self.make_schema(value)
        required = [name for name, f in info["children"].items() if (f.get("freq2") or f["freq"]) == info["freq"]]
        if required:
            d["required"] = required

        # todo: conflict check
        if info.get("type2") == "null":
            d["x-nullable"] = True
        schema_name = self.resolve_name(info)
        self.definitions[schema_name] = d
        return {"$ref": "#/definitions/{name}".format(name=schema_name)}

    def make_primitive_schema(self, info):
        d = OrderedDict(type=info["type"])
        if info["values"]:
            d["example"] = info["values"][0]
        if info.get("type2") == "null":
            d["x-nullable"] = True
        return d

    def emit(self, root):
        return self.make_schema(root)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--name", type=str, default="top")
    parser.add_argument("--annotations", type=argparse.FileType('r'), default=None)
    parser.add_argument("--dst", type=argparse.FileType('w'), default=None)
    parser.add_argument("src", type=argparse.FileType('r'))

    args = parser.parse_args()

    annotations = {}
    if args.annotations:
        annotations = loading.load(args.annotations)

    detector = Detector()
    emitter = Emitter(annotations)

    loading.setup()
    data = loading.load(args.src)
    info = detector.detect(data, args.name)
    # from dictknife import pp
    # pp(info)
    emitter.emit(info)
    loading.dumpfile(emitter.doc, filename=args.dst)
