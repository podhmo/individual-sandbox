import os
import threading
import queue
import minitask
from handofcats import as_subcommand
from minitask.executor.namedpipe import Executor
from minitask.port import namedpipe
from tinyrpc.dispatch import RPCDispatcher

executor = Executor()
dispatcher = RPCDispatcher()


@dispatcher.public("add")
def _add(x: int, y: int) -> int:
    return x + y


@as_subcommand
def run():
    with executor:
        input_endpoint = executor.create_endpoint()
        output_endpoint = executor.create_endpoint()
        executor.spawn(
            worker, input_endpoint=input_endpoint, output_endpoint=output_endpoint
        )

        ipc = minitask.IPC(port=namedpipe)

        def notify():
            with ipc.connect(output_endpoint) as x:
                while True:
                    res = x._recv()
                    if res is None:
                        break
                    print("*********************")
                    print(res.serialize())
                    print("*********************")

        threading.Thread(target=notify).start()

        with ipc.serve(input_endpoint) as x:
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])
            x.send("add", args=[1, 2])


@executor.register
def worker(*, input_endpoint: str, output_endpoint: str):
    ipc = minitask.IPC(port=namedpipe)
    pid = os.getpid()

    q = queue.Queue()

    def peek():
        with ipc.connect(input_endpoint) as x:
            for req in x:
                print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                print("PUT", req.serialize())
                print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                q.put(req)
        q.put(None)

    th = threading.Thread(target=peek)
    th.start()

    with ipc.serve(output_endpoint) as x:
        while True:
            req = q.get()
            if req is None:
                q.task_done()
                break

            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            print("req", req.serialize())
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            import time
            time.sleep(0.1)
            res = dispatcher.dispatch(req)
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            print("res", res.serialize())
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            x._send(res)
            q.task_done()
    q.join()


as_subcommand.run()
