import itertools

argv = ["x", "y", "z", "--", "a", "b", "c", "--", "1", "--", "2"]
itr = iter(argv)
while True:
    parsed = list(itertools.takewhile(lambda x: x != "--", itr))
    if not parsed:
        break
    print(parsed)
print("end")
