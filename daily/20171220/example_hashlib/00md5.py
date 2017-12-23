import hashlib

with open("xxx", "rb") as rf:
    b = rf.read(-1)

print(hashlib.md5(b).hexdigest())

