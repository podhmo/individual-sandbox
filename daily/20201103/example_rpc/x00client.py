import sys
import xmlrpc.client
from handofcats import as_command


@as_command
def run(port: int = 8888) -> None:
    print(f"connect ... {port}", file=sys.stderr)
    s = xmlrpc.client.ServerProxy(f"http://localhost:{port}")
    print(s.pow(2, 3))  # Returns 2**3 = 8
    print(s.add(2, 3))  # Returns 5
    print(s.mul(5, 2))  # Returns 5*2 = 10

    # Print list of available methods
    print(s.system.listMethods())
