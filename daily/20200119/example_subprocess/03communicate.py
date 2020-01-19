import multiprocessing
import logging
from handofcats import as_subcommand

logger = logging.getLogger(__name__)


@as_subcommand
def master() -> None:
    q = multiprocessing.JoinableQueue()
    with multiprocessing.Pool(maxtasksperchild=4) as pool:
        futs = []
        for i in range(10):
            q.put(i)
            fut = pool.apply_async(worker, kwds={"x": i})
            futs.append(fut)

        while futs:
            new_futs = []
            for fut in futs:
                try:
                    logger.info("oo, %r", fut.get(timeout=0.3))
                    q.task_done()
                except multiprocessing.TimeoutError as e:
                    logger.info("timeout: %r", e)
                    new_futs.append(fut)
                except Exception as e:
                    logger.warning("hmm: %r", e, exc_info=True)
                    q.task_done()
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
