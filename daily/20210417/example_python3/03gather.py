import asyncio
import random


async def get_odd():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([1, 3, 5, 7, 9])


async def get_even():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([0, 2, 4, 6, 8])


async def run():
    odd_tasks = []
    even_task = []
    for _ in range(random.randint(1, 10)):
        choice = random.choice([0, 1])
        if choice == 0:
            odd_tasks.append(get_even())
        else:
            even_task.append(get_odd())

    xs = asyncio.gather(*odd_tasks)
    ys = asyncio.gather(*even_task)
    return await asyncio.gather(xs, ys)


results = asyncio.run(run())
print(results)
