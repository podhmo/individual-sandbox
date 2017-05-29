class Magic(object):
    def __getattr__(self, name):
        return Callable(0)


class Callable(int):
    def __call__(self, *arguments):
        print(arguments[0])


m = Magic()
print(m.x)
m.x = 10
print(m.x)
m.test(20)
print(vars(m))
