import json
from collections import UserDict


class RestrictStore(UserDict):
    def __init__(self, name, *args, **kwargs):
        self.name = name
        self.restricted = False
        super().__init__(*args, **kwargs)
        self.restricted = True

    def __setitem__(self, k, v):
        if self.restricted and k not in self.data:
            raise KeyError("{!r} is not found in {!r}".format(k, list(self.data.keys())))
        else:
            self.data[k] = v

    def update(self, *args, **kwargs):
        try:
            self.restricted = False
            super().update(*args, **kwargs)
        finally:
            self.restricted = True


class OpenStruct(object):
    def __init__(self, name, **kwargs):
        self.__dict__["store"] = RestrictStore(name)
        self.update(kwargs)

    def __repr__(self):
        return "<{} data={!r}>".format(self.__dict__["store"].name, self.__dict__["store"])

    def __setattr__(self, k, v):
        try:
            self.store[k] = v
        except KeyError as e:
            raise AttributeError(str(e))

    def __str__(self):
        return json.dumps({self.store.name: self.store.data}, indent=2, ensure_ascii=False)

    def as_dict(self):
        return dict(self.store)

    def update(self, _data=None, **kwargs):
        _data and self.store.update(_data)
        kwargs and self.store.update(kwargs)

    def as_readonly(self):
        return Readonly(self)

    def as_writable(self):
        return self

    def copy(self):
        new = self.__class__(self.store.name)
        new.update(self.store)
        return new


class Readonly(object):
    def __init__(self, ob):
        self.__dict__["ob"] = ob

    def __repr__(self):
        return "<{}(readonly) data={!r}>".format(self.ob.store.name, self.ob.store)

    def __getattr__(self, k):
        return getattr(self.ob, k)

    def __setattr__(self, k, v):
        raise AttributeError("readonly")


def _test():
    obj = OpenStruct(
        'Sukina Tabemono',
        kudamono=r'ringo',
        yasai=r'naganegi',
        okashi=r'kaki no tane')
    print(obj.as_readonly())
    obj.update(nomimono=r'mugicha')
    try:
        obj.niku = r'butaniku'
    except AttributeError as e:
        print(e)
    # copyはwriteableを返すらしい
    obj2 = obj.as_readonly().copy()
    obj2.kudamono = r'kaki'
    print(obj)
    print(obj2)


if __name__ == '__main__':
    _test()
