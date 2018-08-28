import logging
# from jupyter_client import KernelClient
from jupyter_client.blocking import BlockingKernelClient

logging.basicConfig(level=logging.DEBUG)
c = BlockingKernelClient()
msg = c.execute("x = 1")
print("msg", msg)
msg = c.execute("print('hello')")
print("msg", msg)

