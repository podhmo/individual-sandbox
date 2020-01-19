import multiprocessing
import os
import logging
from handofcats import as_subcommand

logger = logging.getLogger(__name__)


@as_subcommand
def master() -> None:
    q = multiprocessing.JoinableQueue()

    def on_finish(x):
        q.task_done()

    def on_error(x):
        q.task_done()

    with multiprocessing.Pool(maxtasksperchild=4) as pool:
        futs = []
        for i in range(10):
            q.put(i)
            fut = pool.apply_async(
                worker, kwds={"x": i}, callback=on_finish, error_callback=on_error
            )
            futs.append(fut)

        while futs:
            new_futs = []
            for fut in futs:
                try:
                    logger.info("oo, %r", fut.get(timeout=0.3))
                except multiprocessing.TimeoutError as e:
                    logger.info("timeout: %r", e)
                    new_futs.append(fut)
            futs = new_futs
    q.join()


def worker(*, x: int):
    import handofcats.customize as c

    c.logging_activate({})

    logger.info("<< %r", x)
    import time

    time.sleep(0.5)
    logger.info(">> %r", x)
    return f"<<{x}>>"


as_subcommand.run()
