import logging
import time
from queue import Empty  # Py 3

from jupyter_client import connect
from jupyter_client import BlockingKernelClient


logging.basicConfig(level=logging.DEBUG)


f = connect.find_connection_file()

print("loaded")

client = BlockingKernelClient()
client.load_connection_file(f)
print("prepared")


# msg_id = client.execute("print('hello')")
msg_id = client.execute("1 + 10")

# client.wait_for_ready()
res = client.get_shell_msg(msg_id, timeout=1)
print(res)
print("----------------------------------------")
msg = res["msg_id"]


for i in range(10):
    if not client.is_alive():
        print("not alived")
        break

    try:
        res = client.get_iopub_msg(msg_id, timeout=1)
    except Empty as e:
        print("!", repr(e))
        time.sleep(0.1)
        continue

    if res["msg_type"] != "status":
        print("!!", res)
        break

    print(res)
    if res["content"]["execution_state"] != "busy":
        print(res)
        print("ok")
        break

    print("hmm", i)

    time.sleep(0.1)

print("end")
# client.complete
# client.inspect
# client.history
# client.shutdown
