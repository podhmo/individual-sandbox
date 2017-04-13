# broken
import yaml
from io import StringIO


def loads(s):
    return yaml.load(StringIO(s))


class Context:
    def __init__(self):
        self.code = {}
        self.frame = []
        self.cont = []

    def push(self, frame):
        self.frame.append(frame)

    def pop(self):
        self.frame.pop()

    def save(self, d, k):
        print("@", d)
        return self.cont.append((d, k))


class CodeWalker(object):
    def __init__(self, ctx):
        self.ctx = ctx

    def walk(self, parser, d, k):
        self.ctx.code[k] = self.walk_body(parser, d[k])

    def walk_body(self, parser, d):
        return d


class DataWalker(object):
    def __init__(self, ctx):
        self.ctx = ctx

    def walk(self, parser, d, k):
        self.ctx.save(d, k)


class Parser(object):
    def __init__(self, ctx):
        self.ctx = ctx
        self.code_walker = CodeWalker(ctx)
        self.data_walker = DataWalker(ctx)

    def parse(self, d):
        if hasattr(d, "keys"):
            for k in list(d.keys()):
                self.ctx.push((d, k))
                self.parse_item(d, k)
                self.ctx.pop()
        elif isinstance(d, (list, tuple)):
            for i in enumerate(d):
                self.ctx.push((d, i))
                self.parse_item(d, i)
                self.ctx.pop()
        else:
            self.parse_value(d)

    def parse_item(self, d, k):
        if k.startswith("$"):
            self.code_walker.walk(self, d, k)
        else:
            self.data_walker.walk(self, d, k)



s = """
$macro:
  $name: 1
  $args: 2
foo:
  bar
"""


def parse(d):
    parser = Parser(Context())
    parser.parse(d)
    return parser.ctx

c = parse(loads(s))
print(c.cont)
print(c.code)
