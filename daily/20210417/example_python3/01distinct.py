import asyncio
import random


async def get_odd():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([1, 3, 5, 7, 9])


async def get_even():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([0, 2, 4, 6, 8])


async def collect_with(awaitable, *, L):
    result = await awaitable
    L.append(result)
    return result


xs, ys = [], []
loop = asyncio.get_event_loop()
tasks = []
for _ in range(random.randint(1, 10)):
    choice = random.choice([0, 1])
    if choice == 0:
        tasks.append(collect_with(get_even(), L=xs))
    else:
        tasks.append(collect_with(get_odd(), L=ys))
results = loop.run_until_complete(asyncio.wait(tasks))
print(xs, ys)
