class StructLogger(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        information = {
            "extra": {
                "kwargs": kwargs,
                "structual": True,
            },
            "stack_info": kwargs.pop("stack_info", False),
            "exc_info": kwargs.pop("exc_info", False),
        }
        return msg, information
