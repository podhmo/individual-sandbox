import typing as t
import typing_extensions as tx
from collections.abc import Sequence

# print(t.Sequence, issubclass(t.Sequence[t.Any], Sequence))
print(vars(t.List[int]))
# {'_inst': False, '_special': False, '_name': 'List', '__origin__': <class 'list'>, '__args__': (<class 'int'>,), '__parameters__': (), '__slots__': None}
print([name for name in dir(t.List[int]) if not name.startswith("_")])
['copy_with']

typ = t.List[int]
print(getattr(typ, "__origin__"), getattr(typ, "__args__"))
