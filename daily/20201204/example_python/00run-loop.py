import asyncio
import logging

flag1 = False
flag2 = False

loop = asyncio.get_event_loop()


async def func1():
    global flag1

    counter_1 = 60

    print("func1")
    while True:
        if flag1:
            counter_1 -= 1
            print(f"func1: {counter_1}")
            if counter_1 == 0:
                print("func1終了")
                flag1 = False
                break
        await asyncio.sleep(1)


async def func2():
    global flag2

    counter_2 = 60

    print("func2")
    while True:
        if flag2:
            counter_2 -= 1
            print(f"func2: {counter_2}")
            if counter_2 == 0:
                print("func2終了")
                flag2 = False
                break
        await asyncio.sleep(1)


def shutdown():
    loop = asyncio.get_event_loop()
    for task in asyncio.all_tasks(loop):
        task.cancel()
    loop.stop()


async def main():
    global flag1
    global flag2

    flag1 = True
    flag2 = False

    def _input() -> str:
        try:
            return input(">")
        except EOFError:
            return "finish"
        # todo: keyboard interrupt?

    print("start process")
    while True:
        loop = asyncio.get_event_loop()
        s = await loop.run_in_executor(None, _input)
        print(f"{s!r}")
        if s == "finish":
            loop.call_soon_threadsafe(shutdown)
            return

        if flag1:
            flag1 = False
            flag2 = True
        else:
            flag1 = True
            flag2 = False


logging.basicConfig(level=logging.DEBUG)
loop.set_debug(True)
loop.create_task(func1())
loop.create_task(func2())
loop.create_task(main())
loop.run_forever()
