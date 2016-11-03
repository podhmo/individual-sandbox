import json


class OpenStruct(object):
    def __init__(self, name, **kwargs):
        self.__dict__["_name"] = name
        self.update(kwargs)

    def __setattr__(self, k, v):
        if k in self.__dict__["_data"]:
            self.__dict__["_data"][k] = v
        else:
            raise AttributeError("{!r} is not found in {!r}".format(k, list(self.__dict__["_data"].keys())))

    def __repr__(self):
        return "<{} data={!r}>".format(self.__class__.__name__, self.__dict__["_data"])

    def as_json(self):
        return json.dumps({self.__class__.__name__: self.__dict__["_data"]}, indent=2, ensure_ascii=False)

    def as_dict(self):
        return dict(self.__dict__["_data"])

    def update(self, _data=None, **kwargs):
        if _data is not None:
            self._update(_data)
        if kwargs:
            self._update(kwargs)

    def _update(self, kwargs):
        if "_data" not in self.__dict__:
            self.__dict__["_data"] = {}
        self.__dict__["_data"].update(kwargs)

    def as_readonly(self):
        return Readonly(self)

    def copy(self):
        new = self.__class__(self._name)
        new.update(self.__dict__["_data"])
        return new


class Readonly(object):
    def __init__(self, ob):
        self.__dict__["ob"] = ob

    def __repr__(self):
        return "<{}(readonly) data={!r}>".format(self.ob._name, self.ob._data)

    def __getattr__(self, k):
        return getattr(self.ob, k)

    def __setattr__(self, k, v):
        raise AttributeError("readonly")

    def as_writable(self):
        return self.ob

    def copy(self):
        return self.__class__(self.ob.copy())


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


if __name__ == '__main__':
    _test()
