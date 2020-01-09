from handofcats import as_command
import logging

logger = logging.getLogger(__name__)


@as_command
def hello(*, name: str = "world") -> None:
    logger.debug("DEBUG")
    logger.info(f"hello {name}")
    logger.warning("WARN")
