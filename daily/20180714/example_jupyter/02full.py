import logging
from jupyter_client.manager import run_kernel

logging.basicConfig(level=logging.DEBUG)

codes = [
    "x = 1 + 1",
    "print('hello', x)",
]

with run_kernel(kernel_name="python") as c:
    for code in codes:
        # https://jupyter-client.readthedocs.io/en/stable/messaging.html?highlight=idle#kernel-status
        # state = busy, idle, starting
        state = "busy"

        msg_id = c.execute(code)
        while state != "idle" and c.is_alive():
            msg = c.get_iopub_msg(timeout=1)
            content = msg.get('content')
            if content is None:
                continue

            if 'execution_state' in content:
                state = content['execution_state']
            print(f"{state}> content: {content}")

"""
DEBUG:traitlets:Starting kernel: ['VENV/bin/python', '-m', 'ipykernel_launcher', '-f', '/tmp/tmp0btfwzuf.json']
DEBUG:traitlets:Connecting to: tcp://127.0.0.1:40589
DEBUG:traitlets:connecting shell channel to tcp://127.0.0.1:47581
DEBUG:traitlets:Connecting to: tcp://127.0.0.1:47581
DEBUG:traitlets:connecting iopub channel to tcp://127.0.0.1:43051
DEBUG:traitlets:Connecting to: tcp://127.0.0.1:43051
DEBUG:traitlets:connecting stdin channel to tcp://127.0.0.1:54195
DEBUG:traitlets:Connecting to: tcp://127.0.0.1:54195
DEBUG:traitlets:connecting heartbeat channel to tcp://127.0.0.1:47571
busy> content: {'execution_state': 'busy'}
busy> content: {'code': 'x = 1 + 1', 'execution_count': 1}
idle> content: {'execution_state': 'idle'}
busy> content: {'execution_state': 'busy'}
busy> content: {'code': "print('hello', x)", 'execution_count': 2}
busy> content: {'name': 'stdout', 'text': 'hello 2\n'}
idle> content: {'execution_state': 'idle'}
"""
