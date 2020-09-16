import sympy
from functools import partial

# init
sympy.init_printing(False, str_printer=sympy.latex)


def print_(*args):
    print(*args)
    args = list(args)
    args[-1] = sympy.latex(args[-1])
    print(*args)


# 展開
print("expand")
x, y = sympy.symbols("x y")
expr = (x + y) ** 3
print_(expr)
print_("->", sympy.expand(expr))
print("")

# 簡約
print("simplify")
expr = (x + x * y) / x
print_(expr)
print_("->", sympy.simplify(expr))
print("")

# 代入
print("subs")
expr = (x + x * y) / x
print_(expr)
print_("->", expr.subs([(y, 2 * x)]))
print("")

# 極限
print("limit")
expr = sympy.sin(x) / x
print_(expr)
print_("->", sympy.limit(expr, x, 0))
print("")

# 微分
print("diff (deriv)")
expr = sympy.sin(x)
print_(expr)
print_("->", sympy.diff(expr, x))

expr = sympy.sin(2 * x)
print_(expr)
print_("->", sympy.diff(expr, x))
print("")

# 級数展開
print("series")
expr = sympy.cos(x)
print_(expr)
print_("->", sympy.series(expr, x))
print("")

# 積分
print("integrate")
expr = 6 * x ** 5
print_(expr)
print_("->", sympy.integrate(expr, x))

expr = sympy.sin(x)
print_(expr)
print_("->", sympy.integrate(expr, x))

expr = sympy.log(x)
print_(expr)
print_("->", sympy.integrate(expr, x))
print("")

# 方程式の求解

print("solve")
expr = x ** 4 - 1
print_(expr)
print_(sympy.solve(expr, x))

expr = x + 5 * y - 2, -3 * x + 6 * y - 15
print_(expr)
print_(sympy.solve(expr, [x, y]))
print("")

# 多項式の場合には factor が代替手段として利用できます.
print("factor")
expr = x ** 4 - 3 * x ** 2 + 1
print_(expr)
print_(sympy.factor(expr))

expr = x ** 4 - 3 * x ** 2 + 1
print_(expr)
print_(sympy.factor(expr, modulus=5))
print("")

# 行列
print("matrix")
A = sympy.Matrix([[1, 0], [0, 1]])
print_(A)

A = sympy.Matrix([[1, x], [y, 1]])
print_(A)

print_(A ** 2)
print("")
