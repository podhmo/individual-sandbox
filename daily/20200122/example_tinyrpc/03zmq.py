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
    sp.terminate()
    print("ok")


@as_subcommand
def server(*, endpoint: str):
    from tinyrpc.transports.zmq import ZmqServerTransport
    from tinyrpc.server import RPCServer
    from tinyrpc.dispatch import RPCDispatcher

    ctx = zmq.Context()
    dispatcher = RPCDispatcher()
    transport = ZmqServerTransport.create(ctx, endpoint)

    rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)
    rpc_server.trace = print

    @dispatcher.public
    def reverse_string(s):
        return s[::-1]

    rpc_server.serve_forever()


@as_subcommand
def client(*, endpoint: str):
    from tinyrpc.transports.zmq import ZmqClientTransport
    from tinyrpc import RPCClient

    ctx = zmq.Context()

    rpc_client = RPCClient(JSONRPCProtocol(), ZmqClientTransport.create(ctx, endpoint))

    remote_server = rpc_client.get_proxy()

    for i in range(3):
        # call a method called 'reverse_string' with a single string argument
        result = remote_server.reverse_string("Hello, World!")

        print("Server answered:", result)


as_subcommand.run()
