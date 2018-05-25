import structlog

getLogger = get_logger = structlog.get_logger
DEFAULT_PROCESSORS = [structlog.processors.JSONRenderer()]


# structlogは、configure()を呼ぶ前にget_logger()されたloggerに関してはconfigが適用されない(ひどい)
def setup(*args, **kwargs):
    kwargs["processors"] = kwargs.pop("processors", DEFAULT_PROCESSORS)
    structlog.configure(*args, **kwargs)
