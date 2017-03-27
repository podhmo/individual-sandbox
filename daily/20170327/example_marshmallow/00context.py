import abc
import unittest
from marshmallow import Schema, fields
from marshmallow import pre_load
from collections import ChainMap


class State(Schema):
    priority = fields.Int(missing=0)

    @classmethod
    def new(cls):
        return {"priority": 0}


class Ob(Schema):
    state = fields.Nested(State, missing=State.new)


class Ob2(Schema):
    state = fields.Nested(State, missing=State.new)

    @pre_load
    def preload(self, data):
        return ChainMap(data, {"state": data})


class Inline(fields.Nested):
    def deserialize(self, value, attr=None, data=None):
        if data is not None:
            value = ChainMap(data, value)
        return super().deserialize(value, attr=attr, data=data)


class Ob3(Schema):
    state = Inline(State, missing=State.new)


class TestBase(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def _makeSchema(self, strict):
        raise NotImplementedError()

    def test_empty(self):
        data = {}
        actual, _ = self._makeSchema(strict=True).load(data)
        self.assertEqual(actual, {"state": {"priority": 0}})

    def test_structured(self):
        data = {"state": {"priority": "1"}}
        actual, _ = self._makeSchema(strict=True).load(data)
        self.assertEqual(actual, {"state": {"priority": 1}})

    def test_flatten(self):
        data = {"priority": "2"}
        actual, _ = self._makeSchema(strict=True).load(data)
        self.assertEqual(actual, {"state": {"priority": 2}})


class Ob2Tests(unittest.TestCase, TestBase):
    def _makeSchema(self, strict):
        return Ob2(strict=strict)


class Ob3Tests(unittest.TestCase, TestBase):
    def _makeSchema(self, strict):
        return Ob3(strict=strict)


if __name__ == "__main__":
    unittest.main()
