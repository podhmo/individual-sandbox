import sys
import typing as t
import threading
import dataclasses
import subprocess
import tempfile
from functools import partial

import zmq
from handofcats import as_subcommand
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.transports.zmq import ZmqClientTransport
from tinyrpc import RPCClient, RPCProxy
from tinyrpc.transports.zmq import ZmqServerTransport
from tinyrpc.server import RPCServer


"""
実行する処理を変えられるようにする。てきとーにtiyrpc(zeromq)を使う
TODO: ipc用のファイル名をいい感じに生成できるようにしたほうが良いかもしれない。
TODO: rpcのinterfaceでは即時で返ってくることを期待するinterface。多分serialize/deseriazeだけ使う?
TODO: tinyrpcの仕様上、同一のendpointの実行中に割り込んでrequestを投げられなそう
TODO: rpc用のendpointを用意するのだとしたら、言語が自由で済む利便性は失われているかもしれない
"""


@as_subcommand
def manager():
    @dataclasses.dataclass
    class Handler:
        uid: int
        process: subprocess.Popen
        remote_server: RPCProxy

    ctx = zmq.Context()
    dispatcher = RPCDispatcher()
    living_process = {None}  # None is semaphore

    @dispatcher.public
    def notify(msg: t.Any):
        print("*", msg, "*")

    @dispatcher.public
    def exit(uid: int):
        print("EXIT", living_process.remove(uid))

    # use tmpfs?
    with tempfile.TemporaryDirectory() as d:
        endpoint = f"ipc://{d}/worker.manager"
        transport = ZmqServerTransport.create(ctx, endpoint)
        rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)

        def server_loop():
            while len(living_process) > 0:
                rpc_server.receive_one_message()

        th = threading.Thread(target=server_loop, daemon=True)
        th.start()

        handlers = []
        n = 3
        protocol = JSONRPCProtocol()

        for uid in range(n):
            living_process.add(uid)
            cmd = [
                sys.executable,
                "-u",
                __file__,
                "worker",
                "--uid",
                str(uid),
                "--endpoint",
                endpoint,
            ]
            p = subprocess.Popen(
                cmd,
                # stdout=subprocess.PIPE,
                # stderr=subprocess.PIPE,
                # text=False,  # for read1
            )
            rpc_client = RPCClient(
                protocol,
                ZmqClientTransport.create(ctx, endpoint.replace(".manager", f".{uid}")),
            )
            remote_server = rpc_client.get_proxy()
            handlers.append(Handler(uid=uid, process=p, remote_server=remote_server))

        # 本当はguardなどを使う
        living_process.remove(None)  # None is semaphore

        futs = []
        from concurrent.futures import ThreadPoolExecutor

        with ThreadPoolExecutor() as ex:
            for h in handlers:
                futs.append(ex.submit(h.remote_server.do_task))
                futs.append(ex.submit(h.remote_server.do_task))

        for fut in futs:
            print(fut.result())

        for h in handlers:
            h.remote_server.exit()
        print("END")
        th.join()


@as_subcommand
def worker(*, uid: int, endpoint: str):
    ctx = zmq.Context()
    dispatcher = RPCDispatcher()

    client = RPCClient(JSONRPCProtocol(), ZmqClientTransport.create(ctx, endpoint),)
    proxy = client.get_proxy()

    @dispatcher.public
    def do_task():
        from time import sleep

        for i in range(5):
            proxy.notify(f"{uid} y{i} ")
            sleep(0.1)

        return f"OK {uid}"

    @dispatcher.public
    def exit():
        proxy.exit(uid)

    transport = ZmqServerTransport.create(ctx, endpoint.replace(".manager", f".{uid}"))
    rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)
    rpc_server.trace = partial(print, file=sys.stderr)
    rpc_server.serve_forever()


as_subcommand.run()
