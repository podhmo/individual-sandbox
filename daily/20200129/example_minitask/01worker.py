import os
import threading
import queue
import minitask
from handofcats import as_subcommand
from minitask.executor.namedpipe import Executor
from minitask.communication import namedpipe
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

        ipc = minitask.IPC(communication=namedpipe)

        def notify():
            with ipc.connect(output_endpoint) as x:
                for res in x:
                    print("*********************")
                    print(res.serialize())
                    print("*********************")

        threading.Thread(target=notify).start()

        with ipc.serve(input_endpoint) as x:
            x.send(x.serialization.create_message("add", args=[1, 2]))
            x.send(x.serialization.create_message("add", args=[1, 2]))


class WorkerQueue:
    def __init__(self, ipc, *, input_endpoint: str):
        self.ipc = ipc
        self.input_endpoint = input_endpoint
        self.q = queue.Queue()

    def peek(self):
        with self.ipc.connect(self.input_endpoint) as x:
            for req in x:
                print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                print("PUT", req.serialize())
                print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                self.q.put(req)
        self.q.put(None)  # finish

    def __iter__(self):
        # TODO: wrap
        th = threading.Thread(target=self.peek)
        th.start()

        while True:
            item = self.q.get()
            if item is None:
                self.q.task_done()
                break
            yield item
            self.q.task_done()
        self.q.join()


@executor.register
def worker(*, input_endpoint: str, output_endpoint: str):
    ipc = minitask.IPC(communication=namedpipe)

    wq = WorkerQueue(ipc, input_endpoint=input_endpoint)
    with ipc.serve(output_endpoint) as x:
        for req in wq:
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            print("req", req.serialize())
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            import time

            time.sleep(0.1)
            res = dispatcher.dispatch(req)
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            print("res", res.serialize())
            print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
            x.send(res)


as_subcommand.run()
