from collections import defaultdict
import sympy


class GenerateSymbols(defaultdict):
    def __missing__(self, key):
        return sympy.Symbol(key)


print(sympy.latex(eval("1+2**(x+y)", GenerateSymbols())))
