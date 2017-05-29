import sys
import os
import signal
from loop import loop


def handle_pdb(sig, frame):
    import pdb
    pdb.Pdb().set_trace(frame)


if __name__ == '__main__':
    signal.signal(signal.SIGUSR1, handle_pdb)
    sys.stdout.write(str(os.getpid()))
    sys.stdout.flush()
    loop()
