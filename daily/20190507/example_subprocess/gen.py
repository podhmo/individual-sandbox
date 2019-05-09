import sys
import time

i = 0
while True:
    print(i)
    sys.stdout.flush()
    time.sleep(0.5)
    i += 1
