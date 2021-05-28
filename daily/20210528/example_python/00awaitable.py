import asyncio


class A:
    def __init__(self, v):
        self.v = v

    def __await__(self):
        try:
            print("hoi >>>", self.v)
            f = asyncio.Future()
            f.set_result(self.v)
            yield from f
            return self.v
        finally:
            print("hai <<<", self.v)


async def run():
    x = await A(10)
    y = await A(200)
    print("@", x, y)
    print(x + y)


asyncio.run(run(), debug=True)

print("----------------------------------------")
asyncio.set_event_loop(asyncio.new_event_loop())
loop = asyncio.get_event_loop()
print(loop.run_until_complete(A(100)))


# print("----------------------------------------")
# asyncio.run(A(1000), debug=True)  # A is awaitable (future like), but not coroutine
