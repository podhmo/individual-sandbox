import time
import logging
from handofcats import as_subcommand
import minitask3 as minitask

logger = logging.getLogger(__name__)
env = minitask.ThreadingEnvironment()
network = minitask.fake_network


@env.register
@as_subcommand
def run(*, keep: bool = False):
    with env:
        pairs = []
        n = 2

        for uid in range(n):
            endpoint = env.create_endpoint(uid=uid)
            sp = env.spawn(producer, endpoint=endpoint)
            cp = env.spawn(consumer, endpoint=endpoint)
            pairs.append((sp, cp))

        for sp, cp in pairs:
            sp.wait()
            cp.wait()


@env.register
def producer(*, endpoint: str):
    import os

    ipc = minitask.IPC(network=network)
    pid = os.getpid()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            # msgpackrpc not support kwargs
            x.send("say", args=["hello", [pid]])
            time.sleep(0.1)


@env.register
def consumer(*, endpoint: str):
    ipc = minitask.IPC(network=network)
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)


as_subcommand.run()
