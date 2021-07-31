import ast
from sympy.parsing.sympy_parser import evaluateFalse


expr = "1 + 2 * x"
t = evaluateFalse(expr)
print(ast.unparse(t))

