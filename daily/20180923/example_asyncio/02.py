import asyncio
from aioknife.synclike import Group


# from https://qiita.com/takechanman1228/items/4cc4728abc8ffbb506fe

# 時間がかかる処理を含む関数
async def factorial(name, number):
    f = 1
    for i in range(2, number + 1):
        print("Task %s: Compute factorial(%s)..." % (name, i))
        await asyncio.sleep(1)
        f *= i

    print("Task %s: factorial(%s) = %s" % (name, number, f))


g = Group()
g.go(factorial, "A", 2)
g.go(factorial, "B", 3)
g.go(factorial, "C", 4)
g.wait()
