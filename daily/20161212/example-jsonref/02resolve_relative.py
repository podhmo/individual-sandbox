# -*- coding:utf-8 -*-
import pprint as p
import jsonref

s = """
{
  "x": {"$ref": "file:../files/x.json#x"},
  "y": {"$ref": "file:../files/y.json#y"}
}
"""

# absolute path is ok
print(s)
data = jsonref.loads(s)
p.pprint(data)
print("failed")
