import asyncio


async def hello():
    await asyncio.sleep(0.1)
    print("hello")


# これは OK
loop = asyncio.get_event_loop()
loop.run_until_complete(hello())
# これも OK
# asyncio.run(hello())

# これは OK
loop = asyncio.get_event_loop()
loop.run_until_complete(asyncio.gather(hello(), hello()))
# # これは NG
# asyncio.run(asyncio.gather(hello(), hello()))
# # ValueError: a coroutine was expected, got <_GatheringFuture pending>
