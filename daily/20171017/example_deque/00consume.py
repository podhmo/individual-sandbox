from collections import deque


def gen():
    for i in range(10):
        print(i)
        yield i


deque(gen(), maxlen=0)
