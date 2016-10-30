import collections
import os.path
import tempfile
import json
import pickle


class PickleLoader(object):
    ext = "pickle"

    def fullpath(self, path):
        return "{}.{}".format(path, self.ext)

    def load(self, path):
        with open(path, "rb") as rf:
            return pickle.load(rf)

    def dump(self, path, data):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "wb") as wf:
            return pickle.dump(data, wf, pickle.HIGHEST_PROTOCOL)


class JSONLoader(object):
    ext = "json"

    def fullpath(self, path):
        return "{}.{}".format(path, self.ext)

    def load(self, path):
        with open(path, "r") as rf:
            return json.load(rf)

    def dump(self, path, data):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as wf:
            return json.dump(data, wf)


class Store(collections.UserDict):
    def __init__(self, manager, name, data):
        self.manager = manager
        self.name = name
        self.data = data

    def save(self, data=None):
        data = data or self.data
        return self.manager.save(self.name, data)

    def load(self):
        return self.manager.load(self.name)


class Manager(object):
    dict_factory = dict
    store_factory = Store

    def __init__(self, root=None, loader=JSONLoader()):
        self.root = root or tempfile.mkdtemp()
        self.loader = loader
        self.stores = {}

    def fullpath(self, name):
        return self.loader.fullpath(os.path.join(self.root, name))

    def load(self, name):
        store = self.stores.get(name)
        if store is None:
            store = self.stores[name] = self.create(name)
        return store

    def save(self, name, data):
        path = self.fullpath(name)
        return self.loader.dump(path, data)

    def create(self, name):
        path = self.fullpath(name)
        if os.path.exists(path):
            data = self.loader.load(path)
        else:
            data = self.dict_factory()
        return self.store_factory(self, name, data)

if __name__ == "__main__":
    manager = Manager("./")
    user_store = manager.load("user")
    user_store["users"] = []
    user_store["users"].append({"name": "foo", "age": 20})
    user_store.save()
