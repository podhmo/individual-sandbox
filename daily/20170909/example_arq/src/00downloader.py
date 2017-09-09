import asyncio
from aiohttp import ClientSession
from arq import Actor, BaseWorker, concurrent


class Downloader(Actor):
    async def startup(self):
        self.session = ClientSession(loop=self.loop)

    @concurrent
    async def download_content(self, url):
        async with self.session.get(url) as response:
            content = await response.read()
            print('{}: {:.80}...'.format(url, content.decode()))
        return len(content)

    async def shutdown(self):
        self.session.close()

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc, tb):
        await self.close()


class Worker(BaseWorker):
    shadows = [Downloader]


async def download_lots():
    async with Downloader() as d:
        tasks = []
        for url in ('https://facebook.com', 'https://microsoft.com', 'https://github.com'):
            tasks.append(d.download_content(url))
        dones, pendings = await asyncio.wait(tasks)
        if len(pendings) > 0:
            raise Exception(pendings)


if __name__ == '__main__':
    loop = asyncio.get_event_loop()
    loop.run_until_complete(download_lots())
