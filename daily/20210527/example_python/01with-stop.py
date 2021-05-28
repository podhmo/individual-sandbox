import time
import sys

# use signal module?

i = 0
try:
    while True:
        try:
            while True:
                print(i)
                time.sleep(0.5)
                i += 1
        except KeyboardInterrupt:
            input("stopped. (if you want to exit, please Ctrl-C)")
except KeyboardInterrupt:
    sys.exit(0)
