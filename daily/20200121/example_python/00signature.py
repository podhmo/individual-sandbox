import typing as t
import inspect
import string

def f() -> str:
    return ""


print(inspect.signature(f))
print(inspect.signature(string.capwords))

if t.TYPE_CHECKING:
    reveal_type(string.capwords)
# 00signature.py:13: note: Revealed type is 'def (s: builtins.str, sep: builtins.str =) -> builtins.str'
