from urllib import parse as p
from dictknife import diff


def is_ascii(u):
    try:
        u.encode("ascii")
        return True
    except:
        return False


def f(s):
    s = s.replace("_", ",")
    return "/".join([x if not x or is_ascii(x[0]) else p.quote(x) for x in s.split("/")]).lower()


with open("/tmp/names.txt") as rf:
    rawdata = rf.readlines()

data = sorted(rawdata, key=f)
for line in diff(rawdata, data):
    print(line)

