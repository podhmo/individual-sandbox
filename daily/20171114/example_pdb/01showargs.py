import sys


def showargs(frame=None, level=1):
    frame = frame or sys._getframe(level)
    co = frame.f_code
    d = frame.f_locals
    n = co.co_argcount
    if co.co_flags & 4:
        n = n + 1
    if co.co_flags & 8:
        n = n + 1
    for i in range(n):
        name = co.co_varnames[i]
        if name in d:
            print('%s = %r' % (name, d[name]))
        else:
            print('%s = *** undefined ***' % (name, ))


def f(x, y, z):
    showargs()
    return (x, y, z)


f(10, 20, 30)
# x = 10
# y = 20
# z = 30
