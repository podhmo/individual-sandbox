import typing as t
import inspect
import string


def f() -> str:
    return ""


spec = inspect.getfullargspec(f)
print(spec)
spec.annotations.update(t.get_type_hints(f))
print("@", spec)

print("----------------------------------------")
spec = inspect.getfullargspec(string.capwords)
print(spec)
spec.annotations.update(t.get_type_hints(string.capwords))
print("@", spec)
