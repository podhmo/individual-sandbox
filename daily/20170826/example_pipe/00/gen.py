import time
import sys
import random
random.seed(0)


for i in range(20):
    print(random.random())
    sys.stdout.flush()
    time.sleep(0.1)
