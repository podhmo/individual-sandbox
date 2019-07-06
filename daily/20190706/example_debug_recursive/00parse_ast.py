from parse import parse_string, PyTreeVisitor

code = """\
def  f(x:int) -> int:
    if x == 0:
        return 1
    else:
        return x * f(x - 1)
"""

t = parse_string(code)
visitor = PyTreeVisitor()
visitor.visit(t)
