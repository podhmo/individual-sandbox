# parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 13, column 2

for c in range(ord("\u0000"), ord("\u001F")):
    print("%r %r %r" % (hex(c), chr(c), chr(c).encode("ascii", "namereplace")))
