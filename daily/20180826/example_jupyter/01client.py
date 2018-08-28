import sys
import textwrap
from logging import getLogger
from jupyter_client import KernelClient
from handofcats import as_command
logger = getLogger(__name__)


def request(bc, code, *, timeout=1):
    # https://jupyter-client.readthedocs.io/en/stable/messaging.html?highlight=idle#kernel-status
    # state = busy, idle, starting
    state = "busy"

    bc.execute(code)
    # todo: whole timeout
    while state != "idle" and bc.is_alive():
        msg = bc.get_iopub_msg(timeout=timeout)
        content = msg.get('content')
        if content is None:
            continue

        if 'execution_state' in content:
            state = content['execution_state']

        logger.debug(f"state=%s, content=%r", state, content)
        if "code" in content:
            logger.info("execution count %d: code=%r", content["execution_count"], content["code"])
        elif "text" in content:
            getattr(sys, content["name"], "stderr").write(content["text"])
        elif "data" in content:
            data = content["data"]
            if "text/plain" in data:
                data = data["text/plain"]
            return data


# @as_command
# def run(*, connection_name: str) -> None:
#     client = KernelClient()
#     client.load_connection_file(connection_name)

#     bc = client.blocking_client()

#     codes = [
#         """
#         g = globals()
#         if "x" not in g:
#             x = 1
#         else:
#             x += 1
#         """,
#         """print(f"hello: {x}")""",
#         """x * x""",
#     ]
#     for code in codes:
#         code = textwrap.dedent(code)
#         data = request(bc, code, timeout=1)
#         if data is not None:
#             print(data)

