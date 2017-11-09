import gc
import math
import psutil
import pandas as pd
import numpy as np


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
df = pd.DataFrame({"foo": np.arange(10000000)})
print("after allocated", convert_size(p.memory_info().rss))
del df
print("after delete variable", convert_size(p.memory_info().rss))
gc.collect()
print("after gc", convert_size(p.memory_info().rss))
import time; time.sleep(1)
print("after sleep", convert_size(p.memory_info().rss))
