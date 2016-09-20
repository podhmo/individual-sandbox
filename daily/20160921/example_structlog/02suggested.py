# https://structlog.readthedocs.io/en/stable/development.html
import logging
import structlog
# import colorama

structlog.configure(
    processors=[
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="%Y-%m-%d %H:%M.%S"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.dev.ConsoleRenderer()  # <===
    ],
    context_class=dict,
    logger_factory=structlog.stdlib.LoggerFactory(),
    wrapper_class=structlog.stdlib.BoundLogger,
    cache_logger_on_first_use=True,
)


logging.basicConfig(level=logging.DEBUG)
logger = structlog.get_logger("hmm")
logger.debug("hello")
logger.info("hello")
logger.warning("hello")
logger.error("hello")
logger.critical("hello")
