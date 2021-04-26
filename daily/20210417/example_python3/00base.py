import asyncio
import random


async def get_odd():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([1, 3, 5, 7, 9])


async def get_even():
    await asyncio.sleep(random.randint(1, 5))
    return random.choice([0, 2, 4, 6, 8])


loop = asyncio.get_event_loop()
tasks = []
for _ in range(random.randint(1, 10)):
    choice = random.choice([0, 1])
    if choice == 0:
        tasks.append(loop.create_task(get_even()))
    else:
        tasks.append(loop.create_task(get_odd()))
results = loop.run_until_complete(asyncio.gather(*tasks))
print(results)
