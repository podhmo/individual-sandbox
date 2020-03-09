import logging
from prestring import NameStore

logger = logging.getLogger(__name__)
# from dictknife.swaggerknife.json2swagger

# todo: non null


def resolve_type(val):
    if isinstance(val, str):
        return "GraphQLString", None
    elif isinstance(val, bool):
        return "GraphQLBool", None
    elif isinstance(val, int):
        return "GraphQLInteger", None
    elif isinstance(val, float):
        return "GraphQLFloat", None
    elif hasattr(val, "keys"):
        return "GraphQLobjectType", None
    elif isinstance(val, (list, tuple)):
        return "GraphQLList", None
    else:
        raise ValueError("unsupported for {!r}".format(val))


class NameResolver:
    def __init__(self):
        self.ns = NameStore()


def make_signature(info):
    if info.get("type2") == "array" or info["type"] == "object":
        return frozenset(
            (k, v["type"], v.get("type2")) for k, v in info["properties"].items()
        )
    else:
        return frozenset((info["type"], info.get("type2")))


class Detector:
    resolve_type = staticmethod(resolve_type)

    def detect(self, d):
        path = []
        reg = {}
        self._detect(d, path=path, ref=reg)
        return reg

    def _detect(self, d, *, path, reg):
        if isinstance(d, (list, tuple)):
            # zero or 1
            pass

        path = [name]
        self._detect(d, s[name], name, path=path)
        return s[name]

    def _detect(self, d, s, name, path):
        if hasattr(d, "keys"):
            s["type"] = "object"
            s["name"] = name
            s["freq"] += 1
            for k, v in d.items():
                if k not in s["properties"]:
                    s["properties"][k] = self.make_info()
                path.append(str(k))
                self._detect(v, s["properties"][k], k, path=path)
                path.pop()
        elif isinstance(d, (list, tuple)):
            s["name"] = name
            s["type2"] = "array"
            s["freq2"] += 1
            for i, x in enumerate(d):
                path.append(str(i))
                self._detect(x, s, name, path=path)  # xxx
                path.pop()
        else:
            if d is None:
                s["type2"] = "null"
            else:
                typ, fmt = self.resolve_type(d)
                s["name"] = name
                s["freq"] += 1
                s["type"] = typ
                if fmt is not None:
                    s["format"] = fmt
                s["values"].append(d)
        ref = "#/{}".format("/".join(path))
        logger.debug("ref %s", ref)
        s["ref"] = ref


data = {"data": {"person": {"name": "foo", "age": 20}}}
d = Detector()
from dictknife import loading

loading.dumpfile(d.detect(data, "Data"))
