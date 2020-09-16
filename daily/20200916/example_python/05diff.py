import sympy

# 偏微分
b0, b1 = sympy.symbols("b0 b1")
N = sympy.symbols("N")
i = sympy.symbols("i", cls=sympy.Idx)
x = sympy.IndexedBase("x")
y = sympy.IndexedBase("y")
# x, y = sympy.symbols("x y", seq=True) # seq?
# x, y = sympy.symbols("x y", cls=sympy.Function)
x, y = sympy.symbols("x y")
L = sympy.Sum((y - b0 - b1 * x) ** 2, (N, 1, i))
print(L)
print(sympy.latex(L))
