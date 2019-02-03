from io import StringIO
import itertools
import contextlib


def peek(files, *, n):
    r = []
    for f in files:
        with contextlib.closing(f) as rf:
            r.append(itertools.islice(rf, n))
    for line in itertools.chain.from_iterable(r):
        print(line.rstrip())


lines = [str(x) for x in range(10)]
files = [StringIO("\n".join(lines)) for i in range(5)]
peek(files, n=3)
