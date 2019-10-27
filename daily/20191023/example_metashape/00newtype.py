import typing as t

ID = t.NewType("ID", str)
print(vars(ID))  # {'__supertype__': <class 'str'>}
ID2 = t.NewType("ID2", ID)
print(
    vars(ID2)
)  # {'__supertype__': <function NewType.<locals>.new_type at 0x7f56e2915f80>}
