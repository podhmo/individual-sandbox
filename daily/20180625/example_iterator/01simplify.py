import itertools


class Resumable:
    def __init__(self, producer, recorder):
        self.producer = producer
        self.recorder = recorder

    def __iter__(self):
        buf = []
        gen_itr = iter(self.producer)
        for subject, cache in itertools.zip_longest(gen_itr, iter(self.recorder)):
            if cache is None:
                buf.append(subject)
                break
            elif cache != subject:
                buf.append(subject)
                break

        for subject in itertools.chain(buf, gen_itr):
            r = yield subject
            if r is None:
                self.recorder(subject)


def producer(xs):
    yield from xs


class Recorder:
    def __init__(self):
        self.mem = []

    def __iter__(self):
        yield from self.mem

    def __call__(self, x):
        self.mem.append(x)


p = producer([1, 2, 3, 4, 5])
c = Recorder()
for x in Resumable(p, c):
    print("@", x)
    if x % 2 == 0:
        break

for x in Resumable(p, c):
    print("!", x)

# @ 1
# @ 2
# ! 3
# ! 4
# ! 5
