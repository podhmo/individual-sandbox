import string

xs = list(string.ascii_lowercase)
bs = [[], []]
for i, x in enumerate(xs):
    if i > 0 and i % 16 == 0:
        for b in bs:
            print(" ".join(b))
        bs = [[], []]
    bs[not bool(i % 4 < 2)].append(x)
for b in bs:
    print(" ".join(b))
