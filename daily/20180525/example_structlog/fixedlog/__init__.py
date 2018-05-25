import structlog


def get_logger(name, *args, **kwargs):
    logger = structlog.get_logger(name, source=name)
    return logger


DEFAULT_PROCESSORS = [
    structlog.processors.JSONRenderer(),
]


def setup(*args, **kwargs):
    kwargs["processors"] = kwargs.pop("processors", DEFAULT_PROCESSORS)
    structlog.configure(*args, **kwargs)
