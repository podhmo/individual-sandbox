from parse import parse_string, PyTreeVisitor

code = """\
# toplevel comments

def  f(x:int) -> int:
    # condition, (inner comments)
    if x == 0:
        return 1
    else:
        return x * f(x - 1)
"""

t = parse_string(code)
visitor = PyTreeVisitor()
visitor.visit(t)
