import sys
import json
from json import scanner

stack = []


class Wrapper:
    def __init__(self, decoder):
        self.decoder = decoder

    def __getattr__(self, name):
        attr = getattr(self.decoder, name)
        if not callable(attr):
            return attr
        if not name.startswith("parse_"):
            return attr

        def wrap(*args, **kwargs):
            stack.append((name, args, kwargs))
            r = attr(*args, **kwargs)
            print(" Input\n\t", name, args, file=sys.stderr)
            print("Output\n\t", name, r, file=sys.stderr)
            return r

        self.__dict__[name] = wrap
        return wrap


class Decoder(json.JSONDecoder):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.scan_once = scanner.py_make_scanner(Wrapper(self))


if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        d = json.load(rf, cls=Decoder)
    print("::", d)
    print(stack)
# parse_string(string, idx + 1, strict)
# parse_string(string, idx + 1, strict)
# parse_object((string, idx + 1), strict,
# parse_array((string, idx + 1), _scan_once)
# parse_float(integer + (frac or '') + (exp or ''))
# parse_int(integer)
# parse_constant('NaN'), idx + 3
