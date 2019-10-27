import typing as t
from metashape.analyze import typeinfo

ID = t.NewType("ID", str)
print(
    typeinfo.detect(ID)
)  # {'raw': <function NewType.<locals>.new_type at 0x7fa3548ddf80>, 'normalized': <function NewType.<locals>.new_type at 0x7fa3548ddf80>, 'underlying': <class 'str'>, 'is_optional': False, 'custom': None, 'supertypes': [<function NewType.<locals>.new_type at 0x7fa3548ddf80>]}
print([c.__name__ for c in typeinfo.detect(ID)["supertypes"]])  # ['ID']

ID2 = t.NewType("ID2", ID)
print(
    typeinfo.detect(ID2)
)  # {'raw': <function NewType.<locals>.new_type at 0x7fa35481cdd0>, 'normalized': <function NewType.<locals>.new_type at 0x7fa35481cdd0>, 'underlying': <class 'str'>, 'is_optional': False, 'custom': None, 'supertypes': [<function NewType.<locals>.new_type at 0x7fa35481cdd0>, <function NewType.<locals>.new_type at 0x7fa3548ddf80>]}
print([c.__name__ for c in typeinfo.detect(ID2)["supertypes"]])  # ['ID2', 'ID']
