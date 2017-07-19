from redbaron import RedBaron

with open("hello.py") as rf:
    t = RedBaron(rf.read())
print(t)

