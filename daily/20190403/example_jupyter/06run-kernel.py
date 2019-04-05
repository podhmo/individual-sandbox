import logging
import json
from jupyter_client import run_kernel
from jupyter_client import BlockingKernelClient

logging.basicConfig(level=logging.DEBUG)

with run_kernel() as c:  # type: BlockingKernelClient
    # c.wait_for_ready(3)
    msg_id = c.execute("print('hello')")
    print((c.get_iopub_msg(msg_id, timeout=1)))
    print((c.get_shell_msg(msg_id, timeout=1)))
    try:
        for i in range(5):
            print((c.get_iopub_msg(msg_id, timeout=1)))
    except Exception as e:
        print(repr(e))

    # shell msg -- for code execution
    # iopub msg -- publish all side effects (stdout, stderr, etc.) (broadcast channel)
    # stdin msg
    # control msg
    # heartbeat msg

    # see also:
    # https://jupyter-client.readthedocs.io/en/stable/messaging.html#the-wire-protocol
    # https://jupyter-client.readthedocs.io/en/stable/messaging.html#heartbeat-for-kernels
