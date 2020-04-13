def foo(x):
    import sys

    ff = sys._getframe(0)
    name = ff.f_code.co_name
    print("call", name)


foo(10)
