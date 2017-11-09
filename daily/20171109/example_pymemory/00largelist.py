import gc
import math
import psutil


def convert_size(size_bytes):
    if size_bytes == 0:
        return "0B"
    size_name = ("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
    i = int(math.floor(math.log(size_bytes, 1024)))
    p = math.pow(1024, i)
    s = round(size_bytes / p, 2)
    return "%s %s" % (s, size_name[i])


p = psutil.Process()
print("before allocated", convert_size(p.memory_info().rss))
L = [object() for L in range(10000000)]
print("after allocated", convert_size(p.memory_info().rss))
del L
print("after delete variable", convert_size(p.memory_info().rss))
gc.collect()
print("after gc", convert_size(p.memory_info().rss))
