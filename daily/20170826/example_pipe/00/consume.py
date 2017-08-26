import sys


for line in sys.stdin:
    print("*", line.strip(), "*")
    sys.stdout.flush()
