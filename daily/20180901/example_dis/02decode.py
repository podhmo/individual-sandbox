from importlib.util import decode_source

with open("./hello.py", "rb") as rf:
    data = rf.read()
print(decode_source(data))
