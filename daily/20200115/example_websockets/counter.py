import time
import sys
import random

for i in range(10):
    n = random.random()
    print(i)
    sys.stdout.flush()
    time.sleep(n)
