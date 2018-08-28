from jupyter_client import KernelClient

# jupyter_client.connect.ConnectionFileMixin.write_connection_fileでfileが出力される
fpath = "/run/user/1000/jupyter/kernel-91949ec0-0f35-4f29-98e9-8c8b4931175c.json"

client = KernelClient()
client.load_connection_file(fpath)
# print(client.get_connection_info())
bc = client.blocking_client()

codes = [
    """x=1""",
    """print(f"hello: {x}")""",
]
for code in codes:
    # https://jupyter-client.readthedocs.io/en/stable/messaging.html?highlight=idle#kernel-status
    # state = busy, idle, starting
    state = "busy"

    msg_id = bc.execute(code)
    while state != "idle" and bc.is_alive():
        msg = bc.get_iopub_msg(timeout=1)
        content = msg.get('content')
        if content is None:
            continue

        if 'execution_state' in content:
            state = content['execution_state']
        print(f"{state}> content: {content}")
