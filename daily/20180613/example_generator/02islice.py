import itertools


def gen():
    for i in range(10):
        print(i)
        yield i


for i in itertools.islice(gen(), 5):
    print("@", i)
