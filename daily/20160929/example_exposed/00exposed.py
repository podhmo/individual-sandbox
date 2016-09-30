# -*- coding:utf-8 -*-
import sys

D = {"a": "b"}

module = sys.modules[__name__]
for k, v in D.items():
    setattr(module, k, v)

print(a)  # => "b"
