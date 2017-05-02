import sys
from dictknife import loading
from prestring import go, PreString
from dictknife.jsonknife.accessor import access_by_json_pointer

"""
- conflict check
- required support
"""


class Emitter:
    DEFAULT_MAPPING = {
        "boolean": "bool",
        "string": "string",
        "number": "float64",
        "integer": "int64",
    }

    def __init__(self, doc, m=None, mapping=None):
        self.doc = doc
        self.m = m or go.Module()
        self.defined = {}  # ref -> name, todo: conflict
        self.type_mapping = mapping or self.DEFAULT_MAPPING

    def resolve_type(self, prop):
        # todo: implement
        typ = prop["type"]
        return self.type_mapping[typ]

    def resolve_tag(self, name):
        return ' `json:"{name}" bson:"{name}"'.format(name=name)

    def emit(self, d, name):
        if "$ref" in d:
            return self.emit_ref(d["$ref"])
        elif d["type"] == "object":
            return self.emit_object(d, name=name)
        elif d["type"] == "array":
            return self.emit_array(d, name=name)
        else:
            raise NotImplementedError(name)

    def emit_object(self, d, name):
        m = self.m.submodule()
        structname = go.goname(name)
        m.comment("{} :{}".format(structname, d.get("description", "")))
        with m.type_(structname, "struct"):
            for name, prop in d["properties"].items():
                if "description" in prop:
                    m.comment("{} :{}".format(go.goname(name), prop.get("description", "")))
                if "$ref" in prop:
                    typename = self.emit_ref(prop["$ref"])
                else:
                    typename = self.resolve_type(prop)
                m.stmt('{} {}{}`'.format(go.goname(name), typename, self.resolve_tag(name)))
        return structname

    def emit_array(self, d, name):
        m = self.m.submodule()
        arrname = go.goname(name)
        item = d["items"]
        if "$ref" in item:
            typename = self.emit_ref(item["$ref"])
        else:
            typename = self.resolve_type(item)
        m.comment("{} :{}".format(arrname, d.get("description", "")))
        m.stmt("type {} []{}".format(arrname, typename))
        m.sep()
        return arrname

    def emit_ref(self, ref):
        if ref in self.defined:
            return self.defined[ref]

        d = access_by_json_pointer(self.doc, ref[1:])
        name = ref.rsplit("/", 1)[-1]
        typename = self.defined[ref] = PreString("")
        typename.append(self.emit(d, name=name))
        return typename


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src", nargs="?", default=sys.stdin, type=argparse.FileType("r"))
    parser.add_argument("--dst", default=sys.stdout, type=argparse.FileType("w"))
    parser.add_argument("--ref", required=True)
    parser.add_argument("--package", default=None)
    args = parser.parse_args()

    doc = loading.load(args.src)
    m = go.Module()
    if args.package:
        m.package(args.package)
    emitter = Emitter(doc, m=m)
    emitter.emit_ref(args.ref)
    print(m)


if __name__ == "__main__":
    main()
