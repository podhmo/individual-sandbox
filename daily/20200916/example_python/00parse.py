from sympy.parsing.latex import parse_latex
from sympy import symbols, sqrt

expr = parse_latex(r"\frac {1 + \sqrt {\a}} {\b}")
a, b = symbols("a b")
print("from latex", expr)
print("from python", (sqrt(a) + 1) / b)
print(expr.evalf(4, subs=dict(a=5, b=2)))
