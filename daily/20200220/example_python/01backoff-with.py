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


# generator or callable

# # we need continuation..
# c = itertools.count()
# with backoff():
#     if next(c) >= 3:
#         print("ok")
#     raise Exception("hmm")

c = itertools.count()
for ref in backoff():
    with ref:
        if next(c) >= 3:
            print("ok")
            break
        raise Exception("hmm")
