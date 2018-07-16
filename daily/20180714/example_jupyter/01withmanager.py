import logging
from jupyter_client.manager import start_new_kernel

logging.basicConfig(level=logging.DEBUG)
manager, client = start_new_kernel()
client.execute("x = 1 + 2")
client.execute("print('hello world', x)")
