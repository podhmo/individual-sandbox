import sys
import asyncio


async def gen(prefix, n, d):
    for i in range(n):
        await asyncio.sleep(d)
        print(f"{prefix}i:{i:04}")
        sys.stdout.flush()


async def main():
    t0 = gen("tag:foo	", 200, d=0.01)
    t1 = gen("tag:bar	", 100, d=0.02)
    t2 = gen("tag:boo	", 10, d=0.1)
    return await asyncio.wait([t0, t1, t2])


asyncio.run(main())
