class FooMixin:
    def foo(self):
        return [f"foo{x}" for x in self.each()]


class Each:
    def each(self):
        return ["x", "y", "z"]


class X(FooMixin, Each):
    pass


class X2(Each, FooMixin):
    pass


print(X().foo())
print(X2().foo())
