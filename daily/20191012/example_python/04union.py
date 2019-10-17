import typing as t
import typing_inspect


def run(typ):
    print(typ)
    print(typing_inspect(typ))


print(t.Union[int, str])
print(t.Union[t.Optional[int], str])
