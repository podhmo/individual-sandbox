import sys
import dis


def f():
    x = 123

    def g():
        print(x)
    return g
# 7           0 LOAD_GLOBAL              0 (print)
#             3 LOAD_DEREF               0 (x)
#             6 CALL_FUNCTION            1 (1 positional, 0 keyword pair)
#             9 POP_TOP
#            10 LOAD_CONST               0 (None)
#            13 RETURN_VALUE

dis.dis(f())


def f2():
    x = 123

    def g():
        frame = sys._getframe()
        print("globals: ", frame.f_globals)
        print("locals: ", frame.f_locals)
        print(x)
    return g

# f_localsの中にclosure()が参照する値も格納されるっぽい
f2()()
