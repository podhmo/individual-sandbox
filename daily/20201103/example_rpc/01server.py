import sys
import pathlib
import time
from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler
from handofcats import as_command


# Restrict to a particular path.
class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = ("/RPC2",)


# Create server
@as_command
def run(*, port: int = 8888, sentinel: str = "") -> None:
    if sentinel:
        p = pathlib.Path(sentinel)
        ok = False
        for wait_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if not p.exists():
                print(f"	wait ... {wait_time}", file=sys.stderr)
                time.sleep(wait_time)
                continue

            print(f"ack {sentinel}", file=sys.stderr)
            p.unlink()
            ok = True
            break
        if not ok:
            raise RuntimeError(f"timeout, f{sentinel}, {time.time()-st}")

    print(f"listen ... {port}", file=sys.stderr)
    with SimpleXMLRPCServer(
        ("localhost", port), requestHandler=RequestHandler
    ) as server:
        server.register_introspection_functions()

        # Register pow() function; this will use the value of
        # pow.__name__ as the name, which is just 'pow'.
        server.register_function(pow)

        # Register a function under a different name
        def adder_function(x, y):
            return x + y

        server.register_function(adder_function, "add")

        # Register an instance; all the methods of the instance are
        # published as XML-RPC methods (in this case, just 'mul').
        class MyFuncs:
            def mul(self, x, y):
                return x * y

        server.register_instance(MyFuncs())

        # Run the server's main loop
        server.serve_forever()
