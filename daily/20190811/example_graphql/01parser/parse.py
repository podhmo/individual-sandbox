from graphql.language.parser import parse
from graphql.pyutils import inspect
from graphql.error import GraphQLSyntaxError
from graphql.language.ast import DocumentNode, Node
from handofcats import as_command


def get_attrs(t: Node, *, bases=set(Node.__dict__.keys()), cache={}):
    cls = t.__class__
    r = cache.get(cls)
    if r is None:
        r = cache[cls] = set(cls.__dict__.keys()).difference(bases)
    return r


def walk(t: Node, *, name="*", depth: int = 0):
    attrs = get_attrs(t)
    print(f'{"  " * depth}{name}<{t.__class__.__name__}>	{sorted(attrs)}')
    depth += 1
    for k in attrs:
        v = getattr(t, k)
        if isinstance(v, Node):
            walk(v, name=k, depth=depth)
        elif isinstance(v, (list, tuple)):
            for i, x in enumerate(v):
                walk(x, name=str(i), depth=depth)
        else:
            print("  " * depth, "@", k, v)


@as_command
def run(filename: str) -> None:
    with open(filename) as rf:
        source = rf.read()
    try:
        t: DocumentNode = parse(source)
        # print(inspect(t))
        walk(t)
    except GraphQLSyntaxError as e:
        print("@", vars(e))
        print("@", e.formatted)
        print("@", e.path,  e.positions)
        print("!!", e)
