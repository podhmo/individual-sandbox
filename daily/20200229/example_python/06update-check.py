import sys
import typing as t
import time
import random
import logging
import threading
from handofcats import as_command, print

logger = logging.getLogger(__name__)


def update_check() -> t.Optional[str]:
    print("-> update_check ...")
    time.sleep(0.5)
    print("<- ... update_check")

    if random.random() < 0.2:
        # update is not found
        return None

    # update is found
    return "0.8.8"


@as_command
def main():
    update_version = None

    def _check():
        nonlocal update_version
        update_version = update_check()

    th = threading.Thread(target=_check, daemon=True)
    th.start()

    print("do something (main)")
    for i in range(6):
        print(".")
        sys.stdout.flush()
        time.sleep(0.1)
    print("ok")

    th.join()
    if update_version is not None:
        print(f"A new release of gh is available: xxx → {update_version}")
