import asyncio
import random


async def get_odd():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([1, 3, 5, 7, 9])


async def get_even():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([0, 2, 4, 6, 8])


xs, ys = [], []
loop = asyncio.get_event_loop()
tasks = []
for _ in range(random.randint(1, 10)):
    choice = random.choice([0, 1])
    if choice == 0:
        t = loop.create_task(get_even())
        tasks.append(t)
        t.add_done_callback(lambda fut: xs.append(fut.result()))
    else:
        t = loop.create_task(get_odd())
        tasks.append(t)
        t.add_done_callback(lambda fut: ys.append(fut.result()))

results = loop.run_until_complete(asyncio.wait(tasks))
print(xs, ys)
