import sys
import json
from json import scanner
from json.decoder import WHITESPACE, WHITESPACE_STR
from json.decoder import scanstring, JSONObject


class Decoder(json.JSONDecoder):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.parse_string = parse_string
        self.parse_object = parse_object
        self.scan_once = scanner.py_make_scanner(self)  # xxx:

    def raw_decode(self, s, idx=0):
        obj, end = super().raw_decode(s, idx=idx)
        print("@", obj, end, file=sys.stderr)
        return obj, end


def parse_string(s: str, idx: int, strict: bool) -> str:
    r = scanstring(s, idx, strict)
    print("@s", repr(s), idx, strict, r, file=sys.stderr)
    return r


def parse_object(
    s_and_end,
    strict,
    scan_once,
    object_hook,
    object_pairs_hook,
    memo=None,
    _w=WHITESPACE.match,
    _ws=WHITESPACE_STR,
):
    r = JSONObject(
        s_and_end,
        strict,
        scan_once,
        object_hook,
        object_pairs_hook,
        memo=memo,
        _w=_w,
        _ws=_ws,
    )
    print("@o", r, file=sys.stderr)
    return r


if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        d = json.load(rf, cls=Decoder)
    print("::", d)

    
