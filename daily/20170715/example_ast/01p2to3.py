from lib2to3 import pytree
from lib2to3 import pygram
from lib2to3.pgen2 import driver
from lib2to3.pgen2 import token

# see: yapf.pytree_utils
_GRAMMAR_FOR_PY3 = pygram.python_grammar_no_print_statement.copy()
del _GRAMMAR_FOR_PY3.keywords['exec']
parser_driver = driver.Driver(_GRAMMAR_FOR_PY3, convert=pytree.convert)


def build_ast_from_string(code):
    driver.Driver(_GRAMMAR_FOR_PY3, convert=pytree.convert)
    return parser_driver.parse_string(code, debug=True)


def name_of(node):
    if node.type < 256:
        return token.tok_name[node.type]
    else:
        return pygram.python_grammar.number2symbol[node.type]


def repr_node(node):
    if isinstance(node, pytree.Node):
        return '%s(%s, %r)' % (
            node.__class__.__name__, name_of(node),
            [repr_node(c) for c in node.children]
        )
    if isinstance(node, pytree.Leaf):
        return '%s(%s, %r)' % (node.__class__.__name__, name_of(node), node.value)


def dump_node(node, indent=0):
    if isinstance(node, pytree.Leaf):
        fmt = '{indent}{name}({value}){prefix}'
        return fmt.format(
            indent=" " * indent,
            name=name_of(node),
            value=repr_node(node),
            prefix=node.prefix,
        )
    else:
        fmt = '{indent}{node}'
        r = [fmt.format(indent=" " * indent, node=name_of(node))]
        for child in node.children:
            r.append(dump_node(child, indent=indent + 2))
        return "\n".join(r)


# import logging
# logging.basicConfig(level=logging.DEBUG)

with open("hello.py") as rf:
    t = build_ast_from_string(rf.read())

print(dump_node(t))
