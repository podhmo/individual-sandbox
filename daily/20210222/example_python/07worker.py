import asyncio
from functools import partial
import fastapi
import uvicorn
from handofcats import as_command


class Worker:
    def __init__(self):
        self.q = asyncio.Queue()
        self.is_running = True
        self.wait_time = 0.1

    async def start(self):
        loop = asyncio.get_event_loop()
        q = self.q

        while self.is_running:
            try:
                name, afn = await asyncio.wait_for(q.get(), self.wait_time)
            except asyncio.TimeoutError:
                continue
            except asyncio.CancelledError:
                raise

            print("dequeue", name)
            # TODO: 同時並行数制御
            t = loop.create_task(afn(), name=name)
            # t.print_stack()
            t.add_done_callback(lambda *args, **kwargs: q.task_done())
        await self.shutdown()

    def stop(self) -> None:
        self.is_running = False

    async def add_item(self, name) -> str:
        prefix = f"task:{name}"
        await self.q.put((prefix, partial(do_task, name)))
        return prefix

    async def shutdown(self) -> None:
        loop = asyncio.get_event_loop()
        # wait_for?
        done, pending = await asyncio.wait(
            [self.q.join()], timeout=10, return_when=asyncio.FIRST_COMPLETED
        )
        if pending:
            print("not finished", pending)
        loop.stop()


async def do_task(prefix):
    for i in range(3):
        print(prefix, i)
        await asyncio.sleep(0.5)
        print(prefix, i, "end")


api = fastapi.FastAPI()
worker = Worker()


@api.get("/")
def hello():
    return {"message": "hello world"}


@api.get("/task")
async def task():
    global worker
    import random

    prefix = await worker.add_item(random.random())
    return {"task": prefix}


@api.on_event("startup")
async def on_startup():
    global worker
    await worker.add_item("start")


@api.get("/shutdown")
def on_shutdown():
    global is_running
    is_running = False
    return {"is_running": is_running}


@as_command
def run():
    loop = asyncio.get_event_loop()

    config = uvicorn.Config(api, port=44444)
    server = uvicorn.Server(config=config)

    loop.create_task(server.serve()).add_done_callback(
        lambda *args, **kwargs: worker.stop()
    )
    loop.create_task(worker.start())
    loop.run_forever()
