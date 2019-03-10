class Alias:
    def __init__(self, name):
        self.name = name


class Meta(type):
    def __new__(self, name, bases, attrs):
        new_attrs = {}
        for k, v in attrs.items():
            if isinstance(v, Alias):
                original_name = v.name
                setattr(
                    self,
                    k,
                    property(
                        fget=lambda s: getattr(s, original_name),
                        fset=lambda s, v: setattr(s, original_name, v),
                    ),
                )
            else:
                new_attrs[k] = v
        return super().__new__(self, name, bases, new_attrs)


class XMeta(Meta):
    pass


class X(metaclass=XMeta):
    a = "aaa"
    b = Alias("a")


print(X.a, X.b)
X.a = "bbb"
print(X.a, X.b)
X.b = "ccc"
print(X.a, X.b)

# -- stdout --------------------
# >> aaa aaa
# >> bbb bbb
# >> ccc ccc
