import sys

controls = tuple([chr(i) for i in range(ord("\u001f"))])

for c in iter(lambda: sys.stdin.read(1), ""):
    if c in controls:
        print(f"\x1b[33m\\e{ord(c)}\x1b[0m", end="")
        if c == "\n":
            print("")
        continue
    print(c, end="")
