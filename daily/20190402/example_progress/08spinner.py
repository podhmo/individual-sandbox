import itertools
import time

frames = itertools.cycle(["🌑 ", "🌒 ", "🌓 ", "🌔 ", "🌕 ", "🌖 ", "🌗 ", "🌘 "])

for i, frame in zip(range(10), frames):
    print(f"\033[1F\033[J{frame}")
    time.sleep(0.3)
