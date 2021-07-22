from sympy import E, pi, ln, cos, Rational, Symbol

# (e^π + 1) * cos(2/7 * π) + ln(3)
expr = ((E ** pi) + 1) * (cos(Rational(2,7) * pi)) + ln(3)

print(expr)
print(expr.evalf())

x = Symbol("x")
print(x * expr)
print((x * expr).evalf(subs={x: 10}))