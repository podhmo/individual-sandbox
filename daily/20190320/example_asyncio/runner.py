import sys
import signal
import subprocess
import argparse


parser = argparse.ArgumentParser()
parser.add_argument("--waittime", default=10, type=int)
parser.add_argument("args", nargs="+")

args = parser.parse_args()
p = subprocess.Popen(args.args, stderr=sys.stderr, stdout=sys.stdout)
print(f"spwan {args.args} (waittime={args.waittime})")
try:
    p.wait(timeout=args.waittime)
except subprocess.TimeoutExpired as e:
    print(f"TIMEOUT: {e}")
    p.send_signal(signal.SIGINT)
