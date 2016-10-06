# -*- coding:utf-8 -*-
import yaml
from io import StringIO

s = """
foo:

bar:
 x: y
"""

data = yaml.load(StringIO(s))
print(data)
# {'foo': None, 'bar': {'x': 'y'}}
