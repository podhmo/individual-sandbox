import time
import logging
from handofcats import as_subcommand
import threading
import minitask3 as minitask

logger = logging.getLogger(__name__)
env = minitask.ThreadingEnvironment()
network = minitask.fake_network

"""
workerっぽいコードを書いてみた。

- input,outputで混乱を招きそう
- workerが終了しきったらおしまいというコードはどうする？
- 内部でqueueとして溜め込みたくない？subprocessのときも(retryiable)。
"""


@as_subcommand
def run(*, keep: bool = False):
    with env:
        n = 1
        ps = []
        for uid in range(n):
            input_endpoint = env.create_endpoint(uid=f"input.{uid}")
            output_endpoint = env.create_endpoint(uid=f"output.{uid}")
            wp = env.spawn(
                worker, input_endpoint=input_endpoint, output_endpoint=output_endpoint
            )
            ps.append(wp)

        ipc = minitask.IPC(network=network)
        with ipc.serve(input_endpoint) as x:
            x.send("say", kwargs={"message": "hello"})

        def peek():
            with ipc.connect(output_endpoint) as x:
                while True:
                    msg = x.recv()
                    if msg is None:
                        break
                    print("!!", msg.serialize())

        th = threading.Thread(target=peek)
        th.start()

        for p in ps:
            p.wait()
        th.join()


@env.register
def worker(*, input_endpoint, output_endpoint: str):
    import os

    ipc = minitask.IPC(network=network)
    pid = os.getpid()

    # todo: queueing
    with ipc.serve(output_endpoint) as sender:
        with ipc.connect(input_endpoint) as receiver:
            while True:
                msg = receiver.recv()  # todo: task_done
                if msg is None:
                    break
                print(pid, "got", msg.serialize())
                time.sleep(0.1)
                sender.send("ok", kwargs={"message": msg.kwargs["message"]})


as_subcommand.run()
