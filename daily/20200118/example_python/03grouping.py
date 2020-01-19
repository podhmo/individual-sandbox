import itertools

L = ["foo", "00", "-", "bar", "01", "-", "foo", "02"]
itr = iter(L)
print(list(itertools.takewhile(lambda x: x != "-", itr)))
print(list(itertools.takewhile(lambda x: x != "-", itr)))
print(list(itertools.takewhile(lambda x: x != "-", itr)))
print(list(itertools.takewhile(lambda x: x != "-", itr)))
