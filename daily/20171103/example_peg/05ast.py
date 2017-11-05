from collections import namedtuple
from parsimonious import Grammar, NodeVisitor

grammar = Grammar(
    r"""
expr = term (space ('+' / '-') space term)*
term = factor (space ('*' / '/') space factor)*
factor = ('+' / '-')? (number / ('(' space expr space ')'))
number = float / int
int = ~'[0-9]+'
float = ~'[0-9]*' '.' ~'[0-9]+'
space = " "*
"""
)


class Symbol(str):
    def __repr__(self):
        return ":{}:".format(self)


BOP = namedtuple("BOP", "name left right")
UOP = namedtuple("UOP", "name value")
Value = namedtuple("Value", "type value")

SPACE = Symbol("space")


class BuildASTVisitor(NodeVisitor):
    def visit_(self, node, children):
        if not children:
            return node.text

        if len(children) == 1 and hasattr(children[0], "join"):
            return children[0]
        else:
            return children

    def visit_space(self, node, children):
        return SPACE

    def visit_int(self, node, children):
        return Value(type="int", value=node.text)

    def visit_float(self, node, children):
        return Value(type="float", value=node.text)

    def visit_number(self, _, children):
        return children[0]

    def visit_factor(self, _, children):
        flag, number_or_expr = children
        if hasattr(number_or_expr[0], "type"):
            val = number_or_expr[0]
        else:
            _, _, val, _, _ = number_or_expr[0]  # ( :space: <val> :space: )
        if flag == "-":
            return UOP(name="-", value=val)
        else:
            return val

    def visit_term(self, _, children):
        factor, *rest = children
        if rest[0]:
            for xs in rest:
                for (_, op, _, val) in xs:
                    factor = BOP(name=op, left=factor, right=val)
        return factor

    def visit_expr(self, _, children):
        term, *rest = children
        if rest[0]:
            for xs in rest:
                for (_, op, _, val) in xs:
                    term = BOP(name=op, left=term, right=val)
        return term


class Evaluator:
    def __init__(self):
        self.coerce_map = {}

    def eval(self, t):
        cls = type(t)
        if cls == BOP:
            return self.eval_bop(t.name, t.left, t.right)
        elif cls == UOP:
            return self.eval_uop(t.name, t.value)
        elif cls == Value:
            return self.eval_value(t.type, t.value)
        else:
            raise ValueError("invalid node: {}".format(t))

    def eval_value(self, t, v):
        coerce_ = self.coerce_map.get(t)
        if coerce_ is None:
            coerce_ = self.coerce_map[t] = getattr(__builtins__, t)
        return coerce_(v)

    def eval_uop(self, op, val):
        if op == "-":
            return -self.eval(val)
        else:
            raise ValueError("unsupported uop {}".format(op))

    def eval_bop(self, op, val0, val1):
        if op == "-":
            return self.eval(val0) - self.eval(val1)
        elif op == "+":
            return self.eval(val0) + self.eval(val1)
        elif op == "*":
            return self.eval(val0) * self.eval(val1)
        elif op == "/":
            return self.eval(val0) / self.eval(val1)
        else:
            raise ValueError("unsupported bop {}".format(op))


def parse_text(s, *, grammar=grammar):
    return grammar.parse(s)


def parse_tree(t):
    v = BuildASTVisitor()
    return v.visit(t)


def eval_(t):
    ev = Evaluator()
    return ev.eval(t)


def run(s):
    print("input : ", s)
    r = eval_(parse_tree(parse_text(s)))
    print("output: ", r)
    return r


run("1")
run("1000000000")
run("1.2")
run("-1.2")
run("1 + 2")
run("1 + 2 + 3")
run("(2 + 3)")
run("1 + (2 + 3)")
run("((2 + 3) + 4)")
run("((2 + 3) + (4 + 5))")
run("2 * ((2 + 3) - (4 + 5))")
run("20 * 1000")
run("2*((3 + 4) - (10 + 2))")
run("-(4-1)*5+(2+4.67)+5.89/(.2+7)")
