from sympy.abc import x
from sympy import simplify

a = (x + 1) ** 2
b = x ** 2 + 2 * x + 1
print(simplify(a - b))

c = x ** 2 - 2 * x + 1
print(simplify(a - c))
