import sys
import time
import signal
import traceback


def handler(signum, frame):
    limit = None
    file = None
    traceback.print_list(traceback.extract_stack(frame, limit=limit), file=file)
    print(signum, frame)
    print(dir(frame))
    sys.exit(-1)


if __name__ == "__main__":
    i = 0
    signal.signal(signal.SIGINT, handler)
    while True:
        print(i)
        time.sleep(0.5)
        i += 1
