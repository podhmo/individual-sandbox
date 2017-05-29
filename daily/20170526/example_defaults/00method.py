class A:
    x = 1

    def f(self, x=x):
        return ("f", x)

    x = 10

    def g(self, x=x):
        return ("g", x)

    x = 100

    def h(self, x=x):
        return ("h", x)


a = A()
print(a.f())
print(a.g())
print(a.h())
