import asyncio
import fastapi
import uvicorn
from handofcats import as_command

api = fastapi.FastAPI()
q = asyncio.Queue()
is_running = True


async def worker():
    global q
    global is_running
    loop = asyncio.get_event_loop()

    print("**START")
    while is_running:
        try:
            prefix = await asyncio.wait_for(q.get(), 0.1)
        except asyncio.TimeoutError:
            continue

        print("dequeue", prefix)
        t = loop.create_task(do_task(prefix))
        t.add_done_callback(lambda *args, **kwargs: q.task_done())

    # wait_for?
    await asyncio.wait([q.join()], timeout=10, return_when=asyncio.FIRST_COMPLETED)
    loop.stop()


async def do_task(prefix):
    for i in range(3):
        print(prefix, i)
        await asyncio.sleep(0.5)
        print(prefix, i, "end")


@api.get("/")
def hello():
    return {"message": "hello world"}


@api.get("/task")
async def task():
    global q
    import random

    prefix = f"task:{random.random()}"
    await q.put(prefix)
    return {"task": prefix}


@api.on_event("startup")
async def on_startup():
    prefix = "task:start"
    await q.put(prefix)


@api.get("/shutdown")
def on_shutdown():
    global is_running
    is_running = False
    return {"is_running": is_running}


@as_command
def run():
    loop = asyncio.get_event_loop()

    # あんまりキレイじゃないけれど..process間通信的な物が欲しい

    config = uvicorn.Config(api, port=44444)
    server = uvicorn.Server(config=config)

    loop.create_task(server.serve()).add_done_callback(
        lambda *args, **kwargs: loop.stop()
    )
    loop.create_task(worker())
    loop.run_forever()
