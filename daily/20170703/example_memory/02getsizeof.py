import sys
from pympler.asizeof import asizeof


class A:
    pass


class B:
    def __init__(self):
        self.a = A()


# https://stackoverflow.com/questions/1331471/in-memory-size-of-a-python-structure
def get_size(obj, seen=None):
    """Recursively finds size of objects"""
    size = sys.getsizeof(obj)
    if seen is None:
        seen = set()
    obj_id = id(obj)
    if obj_id in seen:
        return 0
    # Important mark as seen *before* entering recursion to gracefully handle
    # self-referential objects
    seen.add(obj_id)
    if isinstance(obj, dict):
        size += sum(get_size(k, seen) + get_size(v, seen) for k, v in obj.items())
    elif hasattr(obj, '__dict__'):
        size += get_size(obj.__dict__, seen)
    elif hasattr(obj, '__iter__') and not isinstance(obj, (str, bytes, bytearray)):
        size += sum(get_size(i, seen) for i in obj)
    return size


print([object(), A(), B()])
print([sys.getsizeof(object()), sys.getsizeof(A()), sys.getsizeof(B())])
print([get_size(object()), get_size(A()), get_size(B())])
print([asizeof(object()), asizeof(A()), asizeof(B())])
