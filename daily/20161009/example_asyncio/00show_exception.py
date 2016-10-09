import asyncio

# print(1 / 0)
async def hmm():
    await asyncio.sleep(0.1)
    1 / 0
    await asyncio.sleep(0.5)


async def do_loop():
    asyncio.ensure_future(hmm())
    while True:
        print("hai")
        await asyncio.sleep(1)

if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
