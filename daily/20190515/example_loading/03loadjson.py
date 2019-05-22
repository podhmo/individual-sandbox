import sys
import json
from json.decoder import JSONObject, WHITESPACE, WHITESPACE_STR, JSONDecoder


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


class Decoder(JSONDecoder):
    @property
    def parse_object(self):
        return parse_object

    @parse_object.setter
    def parse_object(self, fn):
        # ignore
        pass


if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        d = json.load(rf, cls=Decoder)
    print("::", d)
