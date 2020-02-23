import time
import itertools


def backoff(fn):
    def _call(*args, **kwargs):
        for wt in [0, 0.1, 0.1, 0.2]:
            try:
                return fn(*args, **kwargs)
            except Exception as e:
                print(e)
                time.sleep(wt)

    return _call


@backoff
def use(c):
    if next(c) >= 3:
        return "ok"
    raise Exception("hmm")


c = itertools.count()
print(use(c))
