from structlog import get_logger

logger = get_logger()
logger.bind(name="alice").info("hai")
