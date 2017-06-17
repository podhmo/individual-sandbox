from dictknife.swaggerknife.json2swagger import Detector as _Detector
from dictknife.swaggerknife.json2swagger import Emitter as _Emitter


class Detector(_Detector):
    def resolve_type(self, value):
        if isinstance(value, str) and value.startswith(("http://", "https://")):
            return "string", "url"
        return super().resolve_type(value)


class Emitter(_Emitter):
    def make_primitive_schema(self, info):
        d = super().make_primitive_schema(info)
        if d.get("format") == "url":
            d["x-go-type"] = "/net/url.URL"
        return d
