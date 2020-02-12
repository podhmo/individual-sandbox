from typing import Union, Optional

tuin = Union[int, None]
tuni = Union[None, int]
toi = Optional[int]

print([str(x) for x in [tuni, tuin, toi]])
print("-")
print(vars(tuin))
print(vars(toi))
