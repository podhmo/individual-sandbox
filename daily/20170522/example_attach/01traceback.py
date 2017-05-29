import sys
import os
import signal
from loop import loop


def handle_traceback(sig, frame):
    import traceback
    traceback.print_stack(limit=5)


if __name__ == '__main__':
    signal.signal(signal.SIGUSR1, handle_traceback)
    sys.stdout.write(str(os.getpid()))
    sys.stdout.flush()
    loop()
