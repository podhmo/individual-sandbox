import time
import re
import gc

rx = re.compile("(\r\n|\r|\n)+\z", re.MULTILINE)
for i in range(20, 24):
    gc.enable()
    t = time.time()
    rx.sub("", "\r\n" * i)
    print(i, time.time() - t)
