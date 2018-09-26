import aioutil
import time


def req(i):
    print(i, "start")
    time.sleep(0.5)
    print(i, "end")
    return i


g = aioutil.Group(limit=5)
for i in range(10):
    g.go(req, i)
print(g.wait())
