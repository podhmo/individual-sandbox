from types import MethodType


class getonly:
    def __init__(self, prop):
        self.prop = prop

    def __get__(self, ob, typ=None):
        if ob is None:
            return self.prop
        v = self.prop(ob)
        ob.__dict__[self.prop.__name__] = v
        return v


class getandset:
    def __init__(self, prop):
        self.prop = prop

    def __get__(self, ob, typ=None):
        if ob is None:
            return self.prop
        v = self.prop(ob)
        ob.__dict__[self.prop.__name__] = v
        return v

    def __set__(self, ob, value):
        raise ValueError("hmm")


class F:
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

    @getonly
    def value(self):
        print("value", "F")
        return 42


class G:
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

    @getandset
    def value(self):
        print("value", "G")
        return 42


f = F()
print(f.value)
print(f.value)
print("----------------------------------------")
g = G()
print(g.value)
print(g.value)
