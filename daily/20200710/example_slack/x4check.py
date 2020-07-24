import asyncio


class Client:
    def __init__(self, loop=None):
        self.loop = loop or asyncio.get_event_loop()

    async def start(self):
        await asyncio.sleep(0.1, loop=self.loop)  # テキトーな処理
        print("started!")


asyncio.run(Client().start())
