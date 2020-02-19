import sys
import re

rx = re.compile(r'[\?<>:\|]')

for line in sys.stdin:
    line = line.rstrip("\n")
    m = rx.search(line)
    if m is None:
        continue
    print(f"git mv {line} {rx.sub('@', line)}")
