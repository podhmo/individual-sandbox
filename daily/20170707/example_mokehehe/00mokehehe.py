import string

# to be:
# a b e f i j m n
# c d g h k l o p
# q r u v y z
# s t w x

xs = iter(list(string.ascii_lowercase))
bufs = [[], []]
try:
    while True:
        for _ in range(4):
            for i in range(2):
                for _ in range(2):
                    bufs[i].append(next(xs))
        for i in range(2):
            print(" ".join(bufs[i]))
            bufs[i] = []
except StopIteration:
    print(" ".join(bufs[0]))
    print(" ".join(bufs[1]))
