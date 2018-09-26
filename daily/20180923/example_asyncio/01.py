from aioknife.synclike import Group
import asyncio


# https://qiita.com/takechanman1228/items/4cc4728abc8ffbb506fe

# 時間がかかる処理を含む関数
async def compute(x, y):
    print("Compute %s + %s ..." % (x, y))
    # 時間がかかる処理にはawaitつける
    await asyncio.sleep(1.0)
    return x + y


g = Group()
g.go(compute, 1, 2)
print(f"1 + 2 = {g.wait()}")
