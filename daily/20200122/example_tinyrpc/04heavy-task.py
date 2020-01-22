from handofcats import as_subcommand
import sys
import zmq
import subprocess
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol


@as_subcommand
def run(*, endpoint: str = "tcp://127.0.0.1:5001"):
    sp = subprocess.Popen([sys.executable, __file__, "server", "--endpoint", endpoint])
    cp = subprocess.Popen([sys.executable, __file__, "client", "--endpoint", endpoint])
    cp.wait()
    sp.wait()
    print("ok")


@as_subcommand
def server(*, endpoint: str):
    import queue
    import threading
    import time
    from concurrent.futures import Future
    from tinyrpc.transports.zmq import ZmqServerTransport
    from tinyrpc.server import RPCServer
    from tinyrpc.dispatch import RPCDispatcher

    q = queue.Queue()
    ev = threading.Event()

    ctx = zmq.Context()
    dispatcher = RPCDispatcher()
    transport = ZmqServerTransport.create(ctx, endpoint)

    rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)
    rpc_server.trace = print

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
def client(*, endpoint: str):
    from tinyrpc.transports.zmq import ZmqClientTransport
    from tinyrpc import RPCClient

    ctx = zmq.Context()

    rpc_client = RPCClient(JSONRPCProtocol(), ZmqClientTransport.create(ctx, endpoint))

    remote_server = rpc_client.get_proxy()

    for i in range(7):
        result = remote_server.act(i, "Hello, World!")
        print("Server answered:", result)

    result = remote_server.shutdown()
    print("Send shutdown:", result)


as_subcommand.run()
