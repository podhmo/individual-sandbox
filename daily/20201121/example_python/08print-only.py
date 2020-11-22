import sys
import bdb
import logging
import linecache
import textwrap
from types import FrameType
import ast

logger = logging.getLogger(__name__)


class _InternalBreak(Exception):
    pass


class _Internal(bdb.Bdb):
    def user_line(self, frame: FrameType):
        sys.settrace(None)

        inner_code = []
        filename = frame.f_code.co_filename
        lineno = frame.f_lineno

        logger.debug("****************************************")
        lv = 0
        max_lines = len(linecache.getlines(filename, frame.f_globals))
        while max_lines > lineno:
            source = linecache.getline(filename, lineno, frame.f_globals)
            next_lv = len(source) - len(source.lstrip(" "))
            logger.debug(source)
            inner_code.append(source)
            if 0 < next_lv < lv:
                break
            if next_lv:
                lv = next_lv
            lineno += 1
        logger.debug("****************************************")

        header = "from prestring.codeobject import Symbol\n"
        t = ast.parse(header + textwrap.dedent("".join(inner_code)))
        Transformer().visit(t)
        exec(compile(t, "<ast>", mode="exec"))
        raise _InternalBreak("ok")
        # linecache.clearcache()


class extract_as_code:
    def __init__(self):
        self.b = _Internal(skip=["bdb"])

    def __enter__(self):
        self.b.set_trace()

    def __exit__(self, typ, val, tb):
        sys.settrace(None)
        if typ is None or isinstance(val, _InternalBreak):
            return True


class Transformer(ast.NodeTransformer):
    def visit_Import(self, node: ast.Import) -> ast.stmt:
        lhs = []
        rhs = []
        for name in node.names:
            if name.asname is None:
                lhs.append(name.name.split(".", 1)[0])
            else:
                lhs.append(name.asname)
            rhs.append(f"Symbol({name.name.split('.', 1)[0]!r})")
        code = f"{', '.join(lhs)} = {', '.join(rhs)}"
        logger.debug("generated code: %s", code)
        t: ast.Module = ast.parse(code)
        assert len(t.body) == 1
        return t.body[0]

    def visit_ImportFrom(self, node: ast.ImportFrom) -> ast.stmt:
        if "prestring" in node.module:
            return node

        # TODO: handling node.module
        lhs = []
        rhs = []
        for name in node.names:
            if name.asname is None:
                lhs.append(name.name.split(".", 1)[0])
            else:
                lhs.append(name.asname)
            rhs.append(f"Symbol({node.module + '.' + name.name.split('.', 1)[0]!r})")
        code = f"{', '.join(lhs)} = {', '.join(rhs)}"
        logger.debug("generated code: %s", code)
        t: ast.Module = ast.parse(code)
        assert len(t.body) == 1
        return t.body[0]


print("outer start")
with extract_as_code():
    from foo import func
    import foo
    import foo.bar, foo.boo as boo

    print(foo)
    print(foo.bar.f())
    print(boo.g())
    print(func.xxx())
print("outer end")
