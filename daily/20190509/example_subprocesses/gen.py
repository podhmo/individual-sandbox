import random
import sys
import time

i = 0
while True:
    out = sys.stdout
    if random.random() > 0.7:
        out = sys.stderr

    print(i, out.fileno(), "hello " * random.randint(1, 10), file=out)
    out.flush()
    time.sleep(random.random())
    i += 1
