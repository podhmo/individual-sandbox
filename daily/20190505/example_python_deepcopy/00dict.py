# pycomment --inplace 00dict.py
import copy

d = {"a": {"b": {"c": 1}}}

# deepcopy
copied = copy.deepcopy(d)
copied["a"]["b"] = "x"
copied # => {'a': {'b': 'x'}}
d # => {'a': {'b': {'c': 1}}}

# copy
copied = copy.copy(d)
copied["a"]["b"] = "x"
copied # => {'a': {'b': 'x'}}
d # => {'a': {'b': 'x'}}
