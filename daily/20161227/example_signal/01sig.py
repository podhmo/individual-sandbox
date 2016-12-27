import sys
import time
import signal


def handler(signum, frame):
    sys.exit(-1)


if __name__ == "__main__":
    i = 0
    signal.signal(signal.SIGINT, handler)
    signal.signal(signal.SIGTERM, handler)
    signal.signal(signal.SIGQUIT, handler)
    signal.signal(signal.SIGHUP, handler)
    while True:
        print(i)
        time.sleep(0.5)
        i += 1
