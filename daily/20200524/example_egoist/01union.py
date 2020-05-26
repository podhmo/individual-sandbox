import typing as t

IntOrStrList = t.List[t.Union[int, str]]
IntOrStrList2 = t.Union[t.List[int], t.List[str]]
print(IntOrStrList, IntOrStrList2)
