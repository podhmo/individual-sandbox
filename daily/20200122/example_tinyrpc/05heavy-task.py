from handofcats import as_subcommand
import sys
import subprocess
import queue
import threading
import time
from concurrent.futures import Future

import zmq
from tinyrpc.transports.zmq import ZmqServerTransport
from tinyrpc.server import RPCServer
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.transports.zmq import ZmqClientTransport
from tinyrpc import RPCClient
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol


@as_subcommand
def run(
    *,
    endpoint: str = "tcp://127.0.0.1:5001",
    callback_endpoint: str = "tcp://127.0.0.1:5002",
):
    sp = subprocess.Popen(
        [
            sys.executable,
            __file__,
            "server",
            "--endpoint",
            endpoint,
            "--callback-endpoint",
            callback_endpoint,
        ]
    )
    cp = subprocess.Popen(
        [
            sys.executable,
            __file__,
            "client",
            "--endpoint",
            endpoint,
            "--callback-endpoint",
            callback_endpoint,
        ]
    )
    cp.wait()
    sp.wait()
    print("ok")


@as_subcommand
def server(*, endpoint: str, callback_endpoint: str):
    q = queue.Queue()
    ev = threading.Event()

    ctx = zmq.Context()
    dispatcher = RPCDispatcher()

    rpc_server = RPCServer(
        ZmqServerTransport.create(ctx, endpoint), JSONRPCProtocol(), dispatcher
    )
    rpc_server.trace = print

    # client
    callback_client = RPCClient(
        JSONRPCProtocol(), ZmqClientTransport.create(ctx, callback_endpoint)
    )
    callback_remote_server = callback_client.get_proxy(one_way=True)

    running = True

    @dispatcher.public
    def act(uid: int, s: str) -> str:
        q.put(("act", (uid, s), {}))
        ev.set()
        return "ok"

    @dispatcher.public
    def shutdown() -> None:
        nonlocal running
        running = False

    def do_act(uid: int, s: str) -> Future:
        fut = Future()

        def do():
            for i in range(5):
                callback_remote_server.notify(f"{uid:02d}: {i} {s}")
                print(f"{uid:02d}: {i} {s}")
                time.sleep(0.1)
            fut.set_result(("ok", uid))

        threading.Thread(target=do, daemon=True).start()
        return fut

    def do_server():
        nonlocal running
        while running:
            rpc_server.receive_one_message()

    def do_loop(*, ns):
        nonlocal running
        while running:
            fn, args, kwargs = q.get()
            fut = ns[f"do_{fn}"](*args, **kwargs)

            def cont(fut):
                q.task_done()

            fut.add_done_callback(cont)

    ns = locals()
    th = threading.Thread(target=do_loop, kwargs={"ns": ns}, daemon=True)
    th.start()
    th2 = threading.Thread(target=do_server, daemon=True)
    th2.start()

    ev.wait()
    q.join()
    # th.join()
    # th2.join()


@as_subcommand
def client(*, endpoint: str, callback_endpoint: str):
    ctx = zmq.Context()

    dispatcher = RPCDispatcher()

    @dispatcher.public
    def notify(s: str):
        print("** {s} **")

    callback_server = RPCServer(
        ZmqServerTransport.create(ctx, callback_endpoint), JSONRPCProtocol(), dispatcher
    )
    callback_server.trace = print
    threading.Thread(target=callback_server.serve_forever, daemon=True).start()

    rpc_client = RPCClient(JSONRPCProtocol(), ZmqClientTransport.create(ctx, endpoint))
    remote_server = rpc_client.get_proxy()

    for i in range(7):
        result = remote_server.act(i, "Hello, World!")
        print("Server answered:", result)

    result = remote_server.shutdown()
    print("Send shutdown:", result)


as_subcommand.run()
