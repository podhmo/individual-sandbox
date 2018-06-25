import itertools


class Resumable:
    def __init__(self, producer, consumer):
        self.producer = producer
        self.consumer = consumer

    def __iter__(self):
        def gen():
            while not self.producer.is_empty():
                yield self.producer.produce()

        def cache_gen():
            yield from self.consumer.history()

        buf = []
        gen_itr = gen()
        for subject, cache in itertools.zip_longest(gen_itr, cache_gen()):
            if cache is None:
                buf.append(subject)
                break
            elif cache != subject:
                buf.append(subject)
                break

        for subject in itertools.chain(buf, gen_itr):
            r = yield subject
            if r is None:
                self.consumer.consume(subject)


class ProducerFromList:
    def __init__(self, xs):
        self.xs = xs
        self.i = 0

    def __iter__(self):
        while not self.is_empty():
            yield self.produce()

    def is_empty(self):
        return self.i >= len(self.xs)

    def produce(self):
        i = self.i
        self.i += 1
        return self.xs[i]


class ConsumerWithMemory:
    def __init__(self):
        self.mem = []

    def history(self):
        return self.mem

    def consume(self, x):
        self.mem.append(x)


p = ProducerFromList([1, 2, 3, 4, 5])
c = ConsumerWithMemory()
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
