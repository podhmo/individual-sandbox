import time
import sys
import random
random.seed(0)


for i in range(20):
    print("{")
    print('"i": {}, "n": {}'.format(i, random.random()))
    print("}")
    time.sleep(0.1)
    sys.stdout.flush()
