import os
import sys
import time
import argparse


parser = argparse.ArgumentParser()
parser.add_argument("--loop", type=int, default=1)
args = parser.parse_args()

for i in range(args.loop):
    print("PID PPID PGID PGRP", file=sys.stderr)
    print(os.getpid(), os.getppid(), os.getpgid(os.getpid()), os.getpgrp())
    if i > 0:
        time.sleep(1)
