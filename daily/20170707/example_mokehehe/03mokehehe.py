import string

it = iter(string.ascii_lowercase)
bs = [[], []]
r = []
try:
    while True:
        for _ in range(4):
            for i in range(2):
                for _ in range(2):
                    bs[i].append(next(it))
        r.extend(bs)
        bs = [[], []]
except StopIteration:
    r.extend(b for b in bs if b)
print("\n".join(" ".join(x for x in xs) for xs in r))
