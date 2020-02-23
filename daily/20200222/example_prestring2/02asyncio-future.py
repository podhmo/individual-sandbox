from prestring.python import PythonModule as Module


m = Module()

m.import_("asyncio")
m.stmt("# from: https://docs.python.org/ja/3/library/asyncio-future.html")
m.sep()

with m.def_("set_after", "fut", "delay", "value", async_=True):
    m.stmt("# Sleep for *delay* seconds.")
    m.stmt("await asyncio.sleep(delay)")

    m.stmt("# Set *value* as a result of *fut* Future.")
    m.stmt("fut.set_result(value)")

with m.def_("main", async_=True):
    m.stmt("# Get the current event loop.")
    m.stmt("loop = asyncio.get_running_loop()")

    m.stmt("# Create a new Future object.")
    m.stmt("fut = loop.create_future()")

    m.stmt('# Run "set_after()" coroutine in a parallel Task.')
    m.stmt('# We are using the low-level "loop.create_task()" API here because')
    m.stmt("# we already have a reference to the event loop at hand.")
    m.stmt('# Otherwise we could have just used "asyncio.create_task()".')

    m.stmt('loop.create_task(set_after(fut, 1, "... world"))')
    m.stmt('print("hello ...")')

    m.stmt("# Wait until *fut* has a result (1 second) and print it.")
    m.stmt("print(await fut)")

m.stmt("asyncio.run(main())")
print(m)
