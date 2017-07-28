from redbaron import RedBaron

with open("src/hello.py") as rf:
    t = RedBaron(rf.read())

print(t.dumps())
