class X:
    def __init__(self):
        self.__dict__["aliases"] = {}

    def __getattr__(self, name):
        original_name = self.aliases.get(name)
        if original_name is None:
            raise AttributeError(name)
        return getattr(self, original_name)

    def __setattr__(self, name, value):
        original_name = self.aliases.get(name)
        if original_name is None:
            self.__dict__[name] = value
        else:
            self.__dict__[original_name] = value

    def register_alias(self, original_name, alias):
        original_name = self.__dict__["aliases"].get(original_name, original_name)
        self.__dict__["aliases"][alias] = original_name


x = X()
x.a = "aaa"
x.register_alias("a", "b")
x.register_alias("b", "c")

print(x.a, x.b, x.c)
x.a = "bbb"
print(x.a, x.b, x.c)
x.b = "ccc"
print(x.a, x.b, x.c)
x.c = "ddd"
print(x.a, x.b, x.c)
# -- stdout --------------------
# >> aaa aaa aaa
# >> bbb bbb bbb
# >> ccc ccc ccc
# >> ddd ddd ddd
