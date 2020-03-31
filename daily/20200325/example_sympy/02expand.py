from sympy.abc import a, b, c, x,y
from sympy import Eq, solve, simplify

expr1 = Eq((c - a) ** 2 + (3 * b) ** 2, 4 * b)
print(expr1, "->", simplify(expr1))
expr2 = Eq((a - b) ** 2 + (3 * c) ** 2, 4 * c)
print(expr2, "->", simplify(expr2))

print(Eq(x-5*y, 3), "->", simplify(Eq(x-5*y, 3)), "solve", solve(Eq(x-5*y, 3), x))
# (c-a)^2 + (3 * b)^2 = 4 * b, (a-b)^2 + (3 * c)^2 = 4 * c, b != c
