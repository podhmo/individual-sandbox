def namedtuple(typename, field_names, *, rename=False, defaults=None, module=None):
    pass


import inspect
from prestring.python.transform import transform_string
print(transform_string(inspect.getsource(namedtuple)))
