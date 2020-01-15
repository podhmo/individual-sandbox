import subprocess
import logging
from functools import partial
from concurrent.futures.thread import ThreadPoolExecutor
from handofcats import as_command

logger = logging.getLogger(__name__)


def do(*, i: int):
    p = subprocess.Popen(["python", "counter.py"], stdout=subprocess.PIPE, text=True)
    for line in p.stdout:
        logger.info("%d: %s", i, line.rstrip())
    p.wait()


@as_command
def run():
    futs = []
    with ThreadPoolExecutor() as ex:
        for i in range(3):
            futs.append(ex.submit(partial(do, i=i)))
    for fut in futs:
        print(fut.result())
