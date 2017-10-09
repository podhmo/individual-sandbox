import os
from collections import OrderedDict
import unittest


def lookup_environ_skip_status(marker):
    name = marker.name.upper()
    is_skip = os.environ.get("NO" + name)
    if is_skip is not None:
        return bool(is_skip)
    return False


class _Marker:
    def __init__(self, name, fn):
        self.name = name
        self.reason = self.name
        self.fn = fn
        self.is_skip = None

    def skip_activate(self):
        self.is_skip = True

    def skip_deactivate(self):
        self.is_skip = False

    def __call__(self, test_item):
        if self.is_skip is None:
            self.is_skip = self.fn(self)
        return unittest.skipIf(self.is_skip, self.reason)(test_item)


class Registry:
    def __init__(self, fn):
        self.pool = OrderedDict()
        self.fn = fn
        self.registered = {}

    def __getattr__(self, name):
        marker = self.pool.get(name)
        if marker is None:
            marker = self.pool[name] = _Marker(name, fn=self.fn)
            if name in self.registered:
                getattr(marker, self.registered[name])()
            elif "" in self.registered:
                getattr(marker, self.registered[""])()
        return marker

    def __iter__(self):
        return iter(self.pool.values())

    def register(self, name, action):
        self.registered[name] = action


markers = Registry(lookup_environ_skip_status)
