from dictknife import loading
from dictknife.jsonknife import access_by_json_pointer

s = """
arguments:
  x: foo
"""

d = loading.loads(s)
print(access_by_json_pointer(d, "#/arguments/x"))

print(access_by_json_pointer(d, "#/arguments/y"))
# Traceback (most recent call last):
#   File "VENV/lib/python3.7/site-packages/dictknife/jsonknife/accessor.py", line 24, in access_by_json_pointer
#     return accessor.access(doc, path)
#   File "VENV/lib/python3.7/site-packages/dictknife/accessing.py", line 15, in access
#     d = d[name]
# KeyError: 'y'

# During handling of the above exception, another exception occurred:

# Traceback (most recent call last):
#   File "00jsonref.py", line 11, in <module>
#     print(access_by_json_pointer(d, "#/arguments/y"))
#   File "VENV/lib/python3.7/site-packages/dictknife/jsonknife/accessor.py", line 26, in access_by_json_pointer
#     raise KeyError(query)
# KeyError: '#/arguments/y'

