from lib2to3 import pytree
from lib2to3.pgen2 import token
from lib2to3 import pygram


def typerepr(node):
    # Nodes with values < 256 are tokens. Values >= 256 are grammar symbols.
    if node.type < 256:
        return token.tok_name[node.type]
    else:
        return pygram.python_grammar.number2symbol[node.type]


class Symbol:
    __slots__ = ("value", )

    def __init__(self, value):
        self.value = value

    def __eq__(self, x):
        if not hasattr(x, "value"):
            return False
        return self.value == x.value

    def __repr__(self):
        return ":{}:".format(self.value)

    mapping = {}

    @classmethod
    def fromstring(cls, s):
        sym = cls.mapping.get(s)
        if sym is None:
            sym = cls.mapping[s] = cls(s)
        return sym


Any = Symbol("Any")
Required = Symbol("Required")  # xxx


class Callable:
    __slots__ = ("args", "keywords", "return_")

    def __init__(self, args, keywords, return_=Any):
        self.args = args
        self.keywords = keywords
        self.return_ = return_

    def __repr__(self):
        return "({self.args}) -> {self.return_}".format(self=self)


class Typed:
    __slots__ = ("name", "type")

    def __init__(self, name, type=Any):
        self.name = name
        self.type = type

    def __repr__(self):
        return "<{self.name} {self.type}>".format(self=self)


def guess_callable(node):
    assert typerepr(node) == "funcdef"
    args, keywords = [], []
    return_ = Any

    parameters = node.children[2]
    assert parameters.children[0].value == "("
    assert parameters.children[-1].value == ")"  # todo -> type
    typedargslist = parameters.children[1]
    # keyrowd*
    if typerepr(typedargslist) == "typedargslist":
        children = typedargslist.children
    else:
        children = [typedargslist]

    if typerepr(node.children[3]) == "RARROW":
        return_ = guess_type(node.children[4])

    is_keyword = False
    is_permanent = False

    for c in children:
        if hasattr(c, "value"):  # leaf
            if c.value in (",", ):
                continue
            elif c.value == "*":
                is_keyword = True
                is_permanent = True
            elif c.value == "=":
                is_keyword = True
            else:
                args.append(guess_symbol(c))
        else:
            if is_keyword:
                if is_permanent:
                    keywords.append((c.value, Required))
                else:
                    keywords.append((args.pop().name, guess_symbol(c)))
                    is_keyword = False
            else:
                args.append(guess_symbol(c))
    # todo: *args, **kwargs
    return Callable(args=args, keywords=keywords, return_=return_)


def guess_type(node):
    try:
        return Symbol.fromstring(node.value)
    except:
        # Node(power, [Leaf(1, 't'), Node(trailer, [Leaf(23, '.'), Leaf(1, 'Callable')]), Node(trailer, [Leaf(9, '['), Node(subscriptlist, [Node(atom, [Leaf(9, '['), Leaf(1, 'int'), Leaf(10, ']')]), Leaf(12, ','), Leaf(1, 'int')]), Leaf(10, ']')])])
        import pdb; pdb.set_trace()


def guess_symbol(node):
    if hasattr(node, "value"):
        return Typed(node.value)
    else:
        typ = typerepr(node)
        if typ == "tname":
            # x : int is
            # Node(tname, [Leaf(1, 'x'), Leaf(11, ':'), Leaf(1, 'int')])
            assert len(node.children) == 3
            name, sep, typ = node.children
            assert sep.value == ":"
            return Typed(name.value, guess_type(typ))
    raise ValueError(node, typerepr(node))


def guess_def(node):
    typ = typerepr(node)
    if typ == "funcdef":
        name = node.children[1].value
        return Typed(name, guess_callable(node))
