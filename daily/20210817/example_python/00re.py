import re

s = "(速度)50(音程)100(話者)男性1"
rx = re.compile(r"\((?P<k>[^ )]+)\)(?P<v>[^ (]+)")

m = rx.search(s)
print(m.groups())
print(m.groupdict())

print([m.groupdict() for m in rx.finditer(s)])

