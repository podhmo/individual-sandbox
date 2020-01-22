import typing as t
import subprocess
import selectors
import threading
import os
import sys
import errno
import logging
from handofcats import as_subcommand

"""
parent pidを見て自分自身を殺す
"""
logger = logging.getLogger(__name__)


@as_subcommand
def manager():
    logger.info("hoi")
    p = subprocess.Popen(
        [
            sys.executable,
            "-u",
            __file__,
            "worker",
            "--uid",
            str(1),
            "--parent-id",
            str(os.getpid()),
        ],
        stdout=subprocess.PIPE,
        # stderr=subprocess.PIPE,
        text=False,  # for read1
    )

    sel = selectors.DefaultSelector()
    sel.register(p.stdout, selectors.EVENT_READ)
    # sel.register(p.stderr, selectors.EVENT_READ)

    def loop():
        while True:
            for key, events in sel.select():
                data = key.fileobj.read1().decode()
                if not data:
                    return
                if key.fileobj is p.stdout:
                    logger.info("STDOUT: %s", data)

    try:
        loop()
    except KeyboardInterrupt:
        exit(0)
    finally:
        p.terminate()


@as_subcommand
def worker(*, uid: int, parent_id: int):
    import sys
    from time import sleep
    from functools import partial

    running = True

    def _exit():
        nonlocal running
        running = False

    th = threading.Thread(
        target=partial(watch_parent_process, pid=parent_id, exit_func=_exit),
        daemon=True,
    )
    th.start()

    while running:
        logger.info("hmm")
        for i in range(10):
            print(f"{uid} x{i} ")
            sleep(0.1)
        sleep(1)


def watch_parent_process(pid: int, *, exit_func: t.Callable[..., None], interval=1):
    if not is_process_alive(pid):
        logger.info("parent process %s is not alive, exiting!", pid)
        exit_func()
    else:
        logger.info("watch")
        # todo: asyncio
        threading.Timer(
            interval,
            watch_parent_process,
            args=[pid],
            kwargs={"interval": interval, "exit_func": exit_func},
        ).start()


# TODO: windows support


def is_process_alive(pid):
    if pid < 0:
        return False
    try:
        os.kill(pid, 0)
    except OSError as e:
        return e.errno == errno.EPERM
    else:
        return True


as_subcommand.run()
