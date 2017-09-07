import time
import asyncio


def task(n):
    print("before:", n)
    time.sleep(0.5)
    print("after:", n)


async def do_task(loop):
    for i in range(20):
        await loop.run_in_executor(None, task, i)


def main():
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_task(loop))
    print("end")


if __name__ == "__main__":
    st = time.time()
    main()
    print(time.time() - st)
