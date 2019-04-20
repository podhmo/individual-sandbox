from dictknife.langhelpers import make_dict
from dictknife.swaggerknife.json2swagger import Detector

# type2,freq2?
# type2 :: array | null
# freq2 :: arrays frequency?


def makeschema(value: dict, *, name: str = "", detector: Detector = None):
    def _on_schema(info, *, from_array=False):
        if not from_array and info.get("type2") == "array":
            return _on_array(info)
        elif info["type"] == "object":
            return _on_object(info)
        else:
            r = _on_primitive(info)
            if r["type"] == "any":
                r.pop("type")
            return r

    def _on_array(info):
        r = make_dict(type="array")
        r["items"] = _on_schema(info, from_array=True)

        # xxx:
        if r["items"].get("type", "any") == "any":
            r.pop("items")
        return r

    def _on_object(info):
        r = make_dict(type="object")
        r["properties"] = make_dict()
        r["required"] = []
        for name, child in info["children"].items():
            r["properties"][name] = _on_schema(child)
            if child.get("freq2") or (child["freq"] == info["freq"]):
                r["required"].append(name)
        if not r["required"]:
            r.pop("required")
        if not r["properties"]:
            r.pop("properties")
        return r

    def _on_primitive(info):
        r = make_dict(type=info["type"])
        if "format" in info:
            r["format"] = info["format"]
        if info["values"]:
            r["example"] = info["values"][0]
        if info.get("type2") == "null":
            r["nullable"] = True
        return r

    detector = detector or Detector()
    detected = detector.detect(value, name)
    return _on_schema(detected)


from handofcats import as_command  # noqa
from dictknife import loading  # noqa


@as_command
def run(*, file: str, name: str = "response") -> None:
    d = loading.loadfile(file)
    loading.dumpfile(makeschema(d, name=name))
