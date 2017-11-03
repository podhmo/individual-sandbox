from parsimonious import Grammar, NodeVisitor
from nbreversible import code


class Visitor(NodeVisitor):
    def __init__(self, key=lambda x: x):
        self.data = {}
        self.key = key

    def generic_visit(self, node, visited_children):
        if node.expr_name and not visited_children:
            self.data[self.key(node.expr_name)] = node.text


# not good performance
grammar = Grammar(
    r"""
dburl = scheme '://' (authinfo '@')? hostinfo? '/' path
scheme = ~"[A-Z 0-9]+"i
authinfo = username ':' password
username = ~"[A-Z 0-9]+"i
password = ~"[A-Z 0-9 \-\.]+"i
hostinfo = (host ':')? port
host = ~"[A-Z 0-9 \.]+"i
port = ~"[1-9][0-9]*"i
path = ~"[A-Z 0-9 /:]+"i
"""
)


def parse(s, prefix="DB_", *, grammar=grammar):
    parsed = grammar.parse(s)
    visitor = Visitor(lambda k: prefix + k.upper())
    visitor.visit(parsed)
    return visitor.data


"""
## examples

"""
with code():
    print(parse("postgres://USER:PASSWORD@HOST:33333/NAME"))
with code():
    print(parse("postgres://HOST:33333/NAME"))
with code():
    print(parse("postgres://USER:PASSWORD@/NAME"))
with code():
    print(parse("sqlite:///:memory:"))
with code():
    print(parse("psql://urser:un-githubbedpassword@127.0.0.1:8458/database"))
