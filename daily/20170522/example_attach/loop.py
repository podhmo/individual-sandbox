import time
import random
import sys


def loop():
    i = 0
    while True:
        print(".", i, file=sys.stderr)
        time.sleep(0.2)
        i += random.random()
        print("..", i, file=sys.stderr)
        time.sleep(0.2)
        i -= random.random()
        print("...", i, file=sys.stderr)
