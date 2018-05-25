import sys
import logging
import structlog

logger = structlog.get_logger()


def f():
    request_id = "xxxxx"
    logger.bind(request_id=request_id).info(
        "user logged in",
        user="test-user",
        context="without new",
    )
    logger.new(request_id=request_id).info(
        "user logged in",
        user="test-user",
        context="with new",
    )


if __name__ == "__main__":
    logging.basicConfig(
        format="**%(message)s**",
        stream=sys.stdout,
        level=logging.INFO,
    )
    structlog.configure(
        processors=[
            structlog.processors.KeyValueRenderer(key_order=["event", "request_id"], ),
        ],
        context_class=structlog.threadlocal.wrap_dict(dict),
    )

    f()
