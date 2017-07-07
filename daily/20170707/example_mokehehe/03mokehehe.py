import string
it = iter(string.ascii_lowercase)
bs = [[], []]
r = []
try:
    while True:
        for _ in range(4):
            for _ in range(2):
                bs[0].append(next(it))
            for _ in range(2):
                bs[1].append(next(it))
        for b in bs:
            r.append(" ".join(b))
            b.clear()
except StopIteration:
    for b in bs:
        r.append(" ".join(b))
print("\n".join(r))
