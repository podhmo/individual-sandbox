import sys
from parselib import parse_from_file, StrictPyTreeVisitor
from lib2to3.pygram import python_symbols as syms
from lib2to3.pgen2 import token


def extract_inner_block(node):
    found = None
    for c in node.children:
        if c.type == syms.suite:
            found = c
            break

    indent_level = 0
    indent = None
    for line in found.children:
        if line.type == token.INDENT:
            indent_level += 1
            indent = line
            continue
        if line.type == token.DEDENT:
            indent_level -= 1
            if indent_level <= 0:
                break
        if indent_level > 0:
            line = line.clone()
            line.prefix = line.prefix.replace(indent.value * indent_level, "")
            yield line


class Visitor(StrictPyTreeVisitor):
    def __init__(self, consume, collector=None):
        self.collector = collector or Collector(consume)

    def visit_file_input(self, node):
        # iterate only toplevel
        for c in node.children:
            self.visit(c)

    def visit_simple_stmt(self, node):
        self.collector.collect(node)

    def visit_decorated(self, node):
        self.collector.collect(node, event=self.collector.events.CODE)

    visit_try_stmt = visit_if_stmt = visit_funcdef = visit_classdef = visit_decorated

    def visit_with_stmt(self, node):
        if node.children[1].children[0].value == "code":
            new = True
            for line in extract_inner_block(node):
                self.collector.collect(line, event=self.collector.events.CODE, new=new)
                if new:
                    new = False
        else:
            self.collector.collect(node, event=self.collector.events.CODE)

    def visit_ENDMARKER(self, node):
        self.collector.consume()


def _surround_with(s, wrapper):
    return s.startswith(wrapper) and s.endswith(wrapper)


class PyCellEvent:
    name = "python"

    def __init__(self, buf=None):
        self.buf = buf or []

    def add(self, stmt):
        self.buf.append(str(stmt))

    def markdown(self, val, file=sys.stdout):
        print("``` {}".format(self.name), file=file)
        print("".join(val).strip(), file=file)
        print("```", file=file)


class MarkdownCellEvent:
    name = "markdown"

    def __init__(self, buf=None):
        self.buf = buf or []

    def add(self, stmt):
        self.buf.append(str(stmt))

    def markdown(self, val, file=sys.stdout):
        print("", file=file)
        print("".join(val).strip("'").strip('"'), file=file)
        print("", file=file)


class Collector:
    class events:
        MARKDOWN = MarkdownCellEvent
        CODE = PyCellEvent
        DEFAULT = PyCellEvent

    def __init__(self, cont, events=events):
        self.cont = cont
        self.prev = None
        self.events = events
        self.current = self.events.DEFAULT()

    def guess_event(self, stmt):
        node = stmt.children[0]
        if node.type == token.STRING and (
            _surround_with(node.value, "'''") or _surround_with(node.value, '"""')
        ):
            return self.events.MARKDOWN
        else:
            return self.events.CODE

    def consume(self):
        if self.current.buf and not getattr(self.current, "_used", False):
            self.current._used = True
            self.cont(self.current, self.current.buf)

    def collect(self, stmt, event=None, new=False):
        event, prev_event = (event or self.guess_event(stmt)), self.prev
        self.prev = event
        if event == self.events.MARKDOWN:
            self.consume()
            self.current = event()
            self.current.add(stmt)
        elif new or prev_event != event:
            self.consume()
            self.current = event()
            self.current.add(stmt)
        else:
            self.current.add(stmt)


def iterate_cell_events(t):
    r = []

    def consume(p, buf):
        r.append((p, buf))

    v = Visitor(consume)
    v.visit(t)
    return iter(r)


t = parse_from_file("./src/sample.py")
for p, buf in iterate_cell_events(t):
    p.markdown(buf)
