import logging
import minitask3 as minitask

logger = logging.getLogger(__name__)
network = minitask.fake_network


def consumer(*, endpoint: str):
    ipc = minitask.IPC(network=network)
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            # 1 / 0
            print("got", msg.unique_id, msg.method, msg.args)


def producer(*, endpoint: str):
    import os

    pid = os.getpid()
    ipc = minitask.IPC(network=network)
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", args=(["hello", [pid]]))


producer(endpoint="xxx")
consumer(endpoint="xxx")
