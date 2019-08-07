import time
import sys
import threading


def tick():
    i = 0
    while True:
        print("tick", i, file=sys.stderr)
        i += 1
        time.sleep(1)


th = threading.Thread(target=tick, daemon=True)
th.start()
# input()
while True:
    time.sleep(10)
