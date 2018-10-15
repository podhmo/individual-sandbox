import sys
import contextlib
from io import StringIO
from lib2to3 import pytree
from lib2to3 import pygram
from lib2to3.pgen2 import driver
from lib2to3.pgen2 import token
from lib2to3.pgen2.parse import ParseError
from lib2to3.fixer_util import Assign, Name, Newline

# utf8 's PUA(https://en.wikipedia.org/wiki/Private_Use_Areas)
SEP = "\U000F0000"

SEP_MARKER = "ZZ{}ZZ".format(SEP)
COMMENT_MARKER = "# =>"
STDOUT_HEADER_MARKER = "# -- stdout --------------------"

default_driver = driver.Driver(pygram.python_grammar_no_print_statement, convert=pytree.convert)


def parse_string(code, parser_driver=default_driver, *, debug=True):
    return parser_driver.parse_string(code, debug=debug)


def parse_file(filename, parser_driver=default_driver, *, debug=True):
    try:
        return parser_driver.parse_file(filename, debug=debug)
    except ParseError as e:
        if "bad input:" not in repr(e):  # work around
            raise
        with open(filename) as rf:
            body = rf.read()
        return parse_string(body + "\n", parser_driver=parser_driver, debug=debug)


def node_name(node):
    # Nodes with values < 256 are tokens. Values >= 256 are grammar symbols.
    if node.type < 256:
        return token.tok_name[node.type]
    else:
        return pygram.python_grammar.number2symbol[node.type]


type_repr = pytree.type_repr


class PyTreeVisitor:
    def visit(self, node):
        method = 'visit_{0}'.format(node_name(node))
        if hasattr(self, method):
            # Found a specific visitor for this node
            if getattr(self, method)(node):
                return

        elif hasattr(node, "value"):  # Leaf
            self.default_leaf_visit(node)
        else:
            self.default_node_visit(node)

    def default_node_visit(self, node):
        for child in node.children:
            self.visit(child)

    def default_leaf_visit(self, leaf):
        pass


def transform_string(source: str):
    t = parse_string(source)
    return transform(t)


def transform_file(fname: str):
    with open(fname) as rf:
        return transform_string(rf.read())


def transform(node):
    t = Transformer()
    t.transform(node)
    return node


class Transformer(PyTreeVisitor):
    marker = COMMENT_MARKER

    def visit_NEWLINE(self, node):
        if node.prefix.lstrip().startswith(self.marker):
            # MEMO: <expr> -> _ = <expr>
            target = node
            while True:
                parent = target.parent
                if parent is None:
                    return

                if type_repr(target.parent.type) == "simple_stmt":
                    break
                target = parent

            eol = target  # target is Leaf("\n]")
            target = eol.prev_sibling

            cloned = target.clone()
            cloned.parent = None

            assigned = Assign(Name("_"), cloned)
            assigned.prefix = target.prefix
            target.replace(assigned)

            # MEMO: adding print(SEP_MARKER, _, SEP_MARKER, sep="\n")
            this_stmt = eol.parent
            print_stmt = this_stmt.clone()
            print_stmt.children = []
            print_stmt.append_child(
                Name(
                    "print({ms!r}, repr(_), {me!r}, sep='')".format(
                        ms="{}{}:".format(SEP_MARKER, node.get_lineno()), me=SEP_MARKER
                    )
                )
            )

            print_stmt.prefix = assigned.prefix
            # xxx: for first line
            if not print_stmt.prefix:
                prev_line = assigned.parent.prev_sibling
                if prev_line.type == token.INDENT:
                    print_stmt.prefix = prev_line.value

            print_stmt.append_child(Newline())

            for i, stmt in enumerate(this_stmt.parent.children):
                if stmt == this_stmt:
                    this_stmt.parent.insert_child(i + 1, print_stmt)
                    break

    transform = PyTreeVisitor.visit


def run(sourcefile, out=sys.stdout):
    o = StringIO()
    with contextlib.redirect_stdout(o):
        exec(str(transform_file(sourcefile)))

    result_map = {}
    stdout_outputs = []
    for line in o.getvalue().splitlines():
        if line.startswith(SEP_MARKER) and line.endswith(SEP_MARKER):
            line = line.strip(SEP_MARKER)
            lineno, line = line.split(":", 2)
            result_map[lineno] = line
        else:
            stdout_outputs.append(line)

    i = 0

    with open(sourcefile) as rf:
        import re
        rx = re.compile(COMMENT_MARKER + ".*$")
        for lineno, line in enumerate(rf, 1):
            if line.rstrip() == STDOUT_HEADER_MARKER:
                break

            m = rx.search(line)
            k = str(lineno)
            if m is None or k not in result_map:
                print(line, end="", file=out)
            else:
                print(line[:m.start()] + COMMENT_MARKER, result_map[k], file=out)
                i += 1

    if stdout_outputs:
        print(STDOUT_HEADER_MARKER, file=out)
        for line in stdout_outputs:
            print("# >>", line, file=out)


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("sourcefile")
    parser.add_argument("--inplace", action="store_true")
    parser.add_argument("--show-only", action="store_true")

    args = parser.parse_args()

    if args.show_only:
        print(str(transform_file(args.sourcefile)))
        from prestring.python.parse import dump_tree
        dump_tree(transform_file(args.sourcefile))
    elif not args.inplace:
        run(args.sourcefile)
    else:
        import tempfile
        import os
        import shutil

        name = None
        try:
            with tempfile.NamedTemporaryFile("w", delete=False) as wf:
                name = wf.name
                run(args.sourcefile, out=wf)
            print("replace: {} -> {}".format(name, args.sourcefile), file=sys.stderr)
            shutil.move(name, args.sourcefile)
        except Exception:
            if os.path.exists(name):
                os.unlink(name)
            raise


if __name__ == "__main__":
    main()
