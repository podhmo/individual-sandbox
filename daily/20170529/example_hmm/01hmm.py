from functools import partial


class Magic(object):
    def __getattr__(self, name):
        return partial(print, 0)

m = Magic()
print(m.x)
m.x = 10
print(m.x)
m.test(20)
print(vars(m))
