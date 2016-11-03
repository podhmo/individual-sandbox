import json


class ClassProvider:
    def __init__(self, base):
        self.base = base
        self.cache = {}

    def create_class(self, name):
        return type(name, (self.base,), {})

    def __call__(self, name, *args, **kwargs):
        cls = self.cache.get(name)
        if cls is None:
            cls = self.cache[name] = self.create_class(name)
        return cls(*args, **kwargs)

    @property
    def __class__(self):
        return self.base


class OpenStructBase(object):
    def __init__(self, **kwargs):
        self.update(kwargs)

    def __setattr__(self, k, v):
        if k in self.__dict__:
            self.__dict__[k] = v
        else:
            raise AttributeError("{!r} is not found in {!r}".format(k, list(self.__dict__.keys())))

    def __repr__(self):
        return "<{} data={!r}>".format(self.__class__.__name__, self.__dict__)

    def as_json(self):
        return json.dumps({self.__class__.__name__: self.__dict__}, indent=2, ensure_ascii=False)

    def as_dict(self):
        return dict(self.__dict__)

    def update(self, _data=None, **kwargs):
        if _data is not None:
            self._update(_data)
        if kwargs:
            self._update(kwargs)

    def _update(self, kwargs):
        self.__dict__.update(kwargs)

    def as_readonly(self):
        return Readonly(self.__class__.__name__, self)

    def copy(self):
        new = self.__class__()
        new.update(self.__dict__)
        return new


class ReadonlyBase(object):
    def __init__(self, ob):
        self.__dict__["ob"] = ob

    def __repr__(self):
        return "<{}(readonly) data={!r}>".format(self.__class__.__name__, self.ob.__dict__)

    def __getattr__(self, k):
        return getattr(self.ob, k)

    def __setattr__(self, k, v):
        raise AttributeError("readonly")

    def as_writable(self):
        return self.ob

    def copy(self):
        return self.__class__(self.ob.copy())


OpenStruct = ClassProvider(OpenStructBase)
Readonly = ClassProvider(ReadonlyBase)


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
    obj2 = obj.as_readonly().copy().as_writable()
    obj2.kudamono = r'kaki'
    print(obj.as_json())
    print(obj2.as_json())

    print(isinstance(obj, OpenStruct.__class__))  # おしい
    print(isinstance(OpenStruct, OpenStructBase))
    print(isinstance(obj, OpenStructBase))

    print(OpenStruct.cache.get("Sukina Tabemono"))

if __name__ == '__main__':
    _test()
