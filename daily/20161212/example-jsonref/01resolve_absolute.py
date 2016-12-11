# -*- coding:utf-8 -*-
import os.path
import pprint as p
import jsonref

s = """
{
  "x": {"$ref": "./files/x.json#x"},
  "y": {"$ref": "./files/y.json#y"}
}
"""

# absolute path is ok
cwd = os.path.dirname(os.path.abspath(__file__))
s = s.replace('"./', '"file://{}/'.format(cwd))
print(s)
data = jsonref.loads(s)
p.pprint(data)
