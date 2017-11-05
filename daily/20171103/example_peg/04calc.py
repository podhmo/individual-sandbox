from parsimonious import Grammar, NodeVisitor

grammar = Grammar(
    r"""
expr = term (space ('+' / '-') space term)*
term = factor (space ('*' / '/') space factor)*
factor = ('+' / '-')? atom
atom = number / ('(' space expr space ')')
number = float / int
int = ~'[0-9]+'
float = ~'[0-9]*' '.' ~'[0-9]+'
space = " "*
"""
)


class Symbol(str):
    def __repr__(self):
        return ":{}:".format(self)


SPACE = Symbol("space")


class Visitor(NodeVisitor):
    # xxx:
    def visit_(self, node, children):
        if children:
            if len(children) == 1 and hasattr(children[0], "join"):
                return children[0]
            else:
                return children
        else:
            return node.text

    def visit_space(self, node, children):
        return SPACE

    def visit_int(self, node, children):
        return int(node.text)

    def visit_float(self, node, children):
        return float(node.text)  # xxx

    def visit_number(self, _, children):
        return children[0]

    def visit_atom(self, _, children):
        if not hasattr(children[0], "__len__"):
            return children[0]
        else:
            return children[0][2]

    def visit_factor(self, _, children):
        flag, number_or_expr = children
        if flag == "-":
            return -number_or_expr
        else:
            return number_or_expr

    def visit_term(self, _, children):
        factor, *rest = children
        if rest[0]:
            for xs in rest:
                for (_, op, _, val) in xs:
                    if op == "*":
                        factor *= val
                    elif op == "/":
                        factor /= val
                    else:
                        raise ValueError("invalid operator: {!r}".format(op))
        return factor

    def visit_expr(self, _, children):
        term, *rest = children
        if rest[0]:
            for xs in rest:
                for (_, op, _, val) in xs:
                    if op == "+":
                        term += val
                    elif op == "-":
                        term -= val
                    else:
                        raise ValueError("invalid operator: {!r}".format(op))
        return term


def parse(s, *, grammar=grammar):
    return grammar.parse(s)


def eval_(t):
    v = Visitor()
    return v.visit(t)


def run(s):
    print("input : ", s)
    r = eval_(parse(s))
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
