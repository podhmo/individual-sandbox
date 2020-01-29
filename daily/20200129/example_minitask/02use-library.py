import threading
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
            x.send(x.serialization.create_message("add", args=[1, 2]))


@executor.register
def worker(*, input_endpoint: str, output_endpoint: str):
    ipc = minitask.IPC(communication=namedpipe)

    with ipc.serve(output_endpoint) as x:
        for req in ipc.connect(input_endpoint):
            print("req", req.serialize())
            import time

            time.sleep(0.5)
            res = dispatcher.dispatch(req)
            print("res", res.serialize())
            x.send(res)


as_subcommand.run()
