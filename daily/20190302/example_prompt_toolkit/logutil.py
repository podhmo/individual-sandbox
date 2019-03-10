import sys
import os.path
import logging
import contextlib
from datetime import datetime

logger = logging.getLogger(__name__)


def setup_logging(
    *,
    logdir=f"./.{os.path.basename(sys.argv[0])}",
    name=None,
    format="%(asctime)s	%(levelname)s	%(name)s	%(message)s",
    filemode="w",
    level=logging.DEBUG,
):
    if logdir is not None:
        os.makedirs(logdir, exist_ok=True)
        name = (
            name
            or f"{len(os.listdir(logdir))+1:04}-{datetime.now().strftime('%Y%m%d')}.log"
        )
        filename = os.path.join(logdir, name)

    logging.basicConfig(
        level=level, filename=filename, filemode=filemode, format=format
    )
    return filename


@contextlib.contextmanager
def run_with_logging(*, setup=setup_logging, logger=logger):
    filename = setup()
    try:
        yield
    except Exception as e:
        logger.error("exception is raised", exc_info=True)
        raise
    finally:
        if filename and os.path.exists(filename) and os.path.getsize(filename) == 0:
            os.remove(filename)
