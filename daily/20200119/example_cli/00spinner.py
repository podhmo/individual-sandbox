import curses
import sys
import time


SPINNER = list(r"-\|/")

curses.setupterm()

for i in range(100):
    print(SPINNER[i % 4])
    print(SPINNER[i % 4])
    print(SPINNER[i % 4])

    sys.stdout.buffer.write(curses.tparm(curses.tigetstr("cuu"), 3))
    sys.stdout.buffer.flush()
    time.sleep(0.1)
