import typing as t
import typing_extensions as tx
import sys
from lib2to3.pytree import Node
from lib2to3.pgen2 import token
from lib2to3 import fixer_util
from prestring.python.parse import parse_string, PyTreeVisitor, type_repr, node_name
import inspect


import hello

# TODO: import
# TODO: conditional
# TODO: return
# TODO: expression全般

Kind = tx.Literal["stmt", "kind", "args"]
changes: t.List[t.Tuple[Kind, Node]] = []


class Visitor(PyTreeVisitor):
    def visit_simple_stmt(self, node: Node):
        if type_repr(node.children[0].type) == "expr_stmt":
            return False

        changes.append(("stmt", node))
        return True  # stop

    def visit_expr_stmt(self, node: Node):
        # <var> = <val>
        if node_name(node.children[1]) == "EQUAL":
            changes.append(("let", node))
            return True  # stop

    def visit_parameters(self, node: Node):
        changes.append(("args", node))


def main():
    code = inspect.getsource(hello)
    code = inspect.getsource(sys.modules[__name__])

    t = parse_string(code)
    print("# before\n", t, sep="")
    v = Visitor()
    v.visit(t)

    for kind, node in changes:
        if kind == "args":
            assert node.children[0].type == token.LPAR
            assert node.children[-1].type == token.RPAR
            node.children.insert(1, fixer_util.Name("m"))
        elif kind == "stmt":
            # TODO: support ";"

            # <stmt>
            # ↓
            # m.stmt(<stmt>)
            node.prefix = f"{node.prefix}m.stmt("
            assert node.children[-1].value == "\n"
            node.children[-1].prefix = f"){node.children[-1].prefix}"

        elif kind == "let":
            if type_repr(node.children[0].type) == "power":
                # TODO: foo.bar.boo  = v =>  setattr(getattr(foo, "bar"), "boo", v)

                # <recv>.<attr> = <val>
                # ↓
                # m.setattr(<recv>, "<attr>", <val>)
                # todo: recursive
                assert type_repr(node.children[0].type) == "power"
                power = node.children[0]
                name = power.children[0].value
                assert type_repr(power.children[1].type) == "trailer"
                trailer = power.children[1]
                assert trailer.children[0].type == token.DOT
                trailer.children[0] = fixer_util.Comma()
                attr = trailer.children[1]

                node.prefix = f"{node.prefix}m.setattr("
                attr.prefix = f'{attr.prefix} "'

                assert node.children[1].type == token.EQUAL
                eq_node = node.children[1]
                node.children[1] = fixer_util.Comma()
                node.children[1].prefix = f'"{eq_node.prefix}'
                node.parent.children[-1].prefix = f"){node.parent.children[-1].prefix}"
                # attr_next.prefix = f'"){attr_next.prefix}'
                continue

            # <var> = <val>
            # ↓
            # <var> = m.let("<var>", <val>)
            name = node.children[0].value
            assert node.children[1].value == "="
            node.children[2].prefix = f"{node.children[2].prefix}m.let({name!r}, "
            next_sibling = node.next_sibling
            assert next_sibling is not None
            next_sibling.prefix = f"){next_sibling}"

    print("")
    print("# after\n", t, sep="")


main()
