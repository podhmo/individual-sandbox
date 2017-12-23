import inspect


def f():
    g()


def g():
    frame = inspect.currentframe().f_back
    print(frame.f_code)
    print(frame.f_code.co_filename, frame.f_code.co_firstlineno, frame.f_lineno)


f()
