import sys
from parselib import parse_from_file, StrictPyTreeVisitor
import buildlib as b
from lib2to3.pgen2 import token


class Visitor(StrictPyTreeVisitor):
    def __init__(self, gen):
        self.gen = gen
        self.collector = Collector(lambda c, v: self.gen.send((c, v)))

    def visit_file_input(self, node):
        # iterate only toplevel
        for c in node.children:
            self.visit(c)

    def visit_simple_stmt(self, node):
        self.collector.collect(node)

    def visit_decorated(self, node):
        self.collector.collect(node, parser=Collector.parsers.CODE)

    def visit_with_stmt(self, node):
        if node.children[1].children[0].value == "code":
            self.collector.collect(node, parser=Collector.parsers.CODE, new=True)
            # to simple statement
            # suite = node.children[3]
            # reading = False
            # stack = []
            # for subnode in suite.children:
            #     if 
            #     if reading:
            #     if subnode.type == b.symbols.simple_stmt:
            #         reading = True
            #         self.collector.collect(node, parser=Collector.parsers.CODE, new=True)
        else:
            self.collector.collect(node, parser=Collector.parsers.CODE)

    def visit_ENDMARKER(self, node):
        self.collector.consume()


def _surround_with(s, wrapper):
    return s.startswith(wrapper) and s.endswith(wrapper)


class PyCellParser:
    name = "python"

    def __init__(self, buf=None):
        self.buf = buf or []

    def collect(self, stmt):
        self.buf.append(str(stmt))

    def markdown(self, val, file=sys.stdout):
        print("``` {}".format(self.name), file=file)
        print("".join(val).rstrip(), file=file)
        print("```", file=file)


class MarkdownCellParser:
    name = "markdown"

    def __init__(self, buf=None):
        self.buf = buf or []

    def collect(self, stmt):
        self.buf.append(str(stmt))

    def markdown(self, val, file=sys.stdout):
        print("", file=file)
        print("".join(val)[3:-4].rstrip(), file=file)
        print("", file=file)


class Collector:
    class parsers:
        MARKDOWN = MarkdownCellParser
        CODE = PyCellParser
        DEFAULT = PyCellParser

    def __init__(self, cont, parser=None):
        self.cont = cont
        self.prev = None
        self.current = parser or self.parsers.DEFAULT()

    def guess_parser(self, stmt):
        node = stmt.children[0]
        if node.type == token.STRING and (
            _surround_with(node.value, "'''") or _surround_with(node.value, '"""')
        ):
            return self.parsers.MARKDOWN
        else:
            return self.parsers.CODE

    def consume(self):
        if self.current.buf and not getattr(self.current, "_used", False):
            self.current._used = True
            self.cont(self.current, self.current.buf)

    def collect(self, stmt, parser=None, new=False):
        parser, prev_parser = (parser or self.guess_parser(stmt)), self.prev
        self.prev = parser
        if parser == self.parsers.MARKDOWN:
            self.consume()
            self.current = parser()
            self.current.collect(stmt)
        elif new or prev_parser != parser:
            self.consume()
            self.current = parser()
            self.current.collect(stmt)
        else:
            self.current.collect(stmt)


def consume():
    while True:
        p, val = yield
        p.markdown(val)


gen = consume()
gen.send(None)
v = Visitor(gen)
t = parse_from_file("./src/sample.py")
v.visit(t)
# print(t.next_sibling)
# print(dir(t))
