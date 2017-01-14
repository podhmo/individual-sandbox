class HandleOptionMeta(type):
    def __new__(self, name, bases, attrs):
        meta = {}
        used = set()
        for b in bases:
            if hasattr(b, "meta"):
                meta.update(b.meta)
                for cls in b.mro():
                    used.add(cls)
            else:
                for cls in reversed(b.mro()):
                    if cls in used:
                        continue
                    used.add(cls)
                    if not hasattr(cls, "Meta"):
                        continue
                    meta.update(cls.Meta.__dict__)
        if "Meta" in attrs:
            meta.update(attrs["Meta"].__dict__)
        attrs.update({"meta": meta})
        return super().__new__(self, name, bases, attrs)


class Base(object):
    def __init__(self, x=None, y=None, z=None):
        self.x = x if x is not None else self.meta["x"]
        self.y = y if y is not None else self.meta["y"]
        self.z = z if z is not None else self.meta["z"]

    def __repr__(self):
        return "{self.__class__.__name__}(x={self.x!r}, y={self.y!r}, z={self.z!r})".format(self=self)


class D(Base, metaclass=HandleOptionMeta):
    class Meta:
        x = 1
        y = 2
        z = 3

print(D())  # D(x=1, y=2, z=3)
print(D(y=20))  # D(x=1, y=20, z=3)


class E(D):
    class Meta:
        x = 10
        y = 20

print(E())  # E(x=10, y=20, z=3)


class F(D):
    class Meta:
        y = 200

print(F(z=3000))  # F(x=1, y=200, z=3000)


class G(D):
    class Meta:
        z = -30


class H(F, G):
    class Meta:
        x = 10000000000


print(H())  # H(x=10000000000, y=2, z=-30)

print(D.Meta.x)  # 1
D.Meta.x = 100
# hmm
print(D())  # D(x=1, y=2, z=3)
