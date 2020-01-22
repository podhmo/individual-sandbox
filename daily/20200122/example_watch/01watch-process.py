import typing as t
import os
import time
import threading
import errno
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)

"""
watch pidを見て自分自身を殺す
"""


def watch_process(pid: int, *, exit_func: t.Callable[..., None], interval=10):
    is_alive = is_process_alive(pid)
    if not is_alive:
        logger.warning("watch process %s is not alive, exiting!", pid)
        return exit_func()
    else:
        logger.warning("watch: pid=%d, alive?=%s", pid, is_alive)
        # todo: asyncio
        threading.Timer(
            interval,
            watch_process,
            args=[pid],
            kwargs={"interval": interval, "exit_func": exit_func},
        ).start()


# TODO: windows support
# https://github.com/palantir/python-language-server/blob/develop/pyls/_utils.py#L155

def is_process_alive(pid):
    if pid < 0:
        return False
    try:
        os.kill(pid, 0)
    except OSError as e:
        logger.warning(e)
        return e.errno == errno.EPERM
    else:
        return True


@as_command
def run(*, pid: int):
    running = True

    def stop():
        nonlocal running
        running = False

    watch_process(pid, exit_func=stop, interval=2)
    while running:
        print(pid, "<-", os.getpid())
        time.sleep(1)
