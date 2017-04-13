# broken
import yaml
from io import StringIO


def loads(s):
    return yaml.load(StringIO(s))


class Parser(object):
    def __init__(self):
        self.code = {}

    def parse(self, d, frameset=None):
        frameset = frameset or []
        if hasattr(d, "keys"):
            for k in list(d.keys()):
                frameset.append((d, k))
                self.parse_item(d, k, frameset=frameset)
                frameset.pop()
        elif isinstance(d, (list, tuple)):
            for i in enumerate(d):
                frameset.append((d, i))
                self.parse_item(d, i, frameset=frameset)
                frameset.pop()
        else:
            self.parse_value(d)
        return d

    def parse_item(self, d, k, frameset):
        if k.startswith("$"):
            self.code[k] = (frameset[:], d.pop(k))
        else:
            self.parse(d[k], frameset=frameset)

    def parse_value(self, d):
        pass


s = """
definitions:
  $macro:
    $name: 1
    $args: 2
foo:
  bar
"""


def parse(d):
    parser = Parser()
    d = parser.parse(d)
    return d, parser.code

c = parse(loads(s))
print(c)
