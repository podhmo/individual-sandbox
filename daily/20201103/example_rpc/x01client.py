import sys
import time
import pathlib
import xmlrpc.client
from handofcats import as_command


@as_command
def run(port: int = 8888, sentinel: str = "") -> None:
    if sentinel:
        p = pathlib.Path(sentinel)
        with p.open("w"):
            pass

        ok = False
        st = time.time()
        for wait_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if p.exists():
                print(f"	wait ... {wait_time}", file=sys.stderr)
                time.sleep(wait_time)
                continue
            ok = True
            break
        if not ok:
            raise RuntimeError(f"timeout, f{sentinel}, {time.time()-st}")

    print(f"connect ... {port}", file=sys.stderr)
    s = xmlrpc.client.ServerProxy(f"http://localhost:{port}")
    print(s.pow(2, 3))  # Returns 2**3 = 8
    print(s.add(2, 3))  # Returns 5
    print(s.mul(5, 2))  # Returns 5*2 = 10

    # Print list of available methods
    print(s.system.listMethods())
