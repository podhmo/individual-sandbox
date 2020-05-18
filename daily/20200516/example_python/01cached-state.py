from functools import lru_cache


@lru_cache(1)
def cached_state():
    print("hello")
    ob = object()
    print("byebye")
    return ob


print(cached_state(), cached_state())
# hello
# byebye
# <object object at 0x101670c60> <object object at 0x101670c60>

