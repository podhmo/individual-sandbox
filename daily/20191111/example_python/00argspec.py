import inspect


def run(filename: str) -> None:
    pass


inspect.getfullargspec(run)  # => FullArgSpec(args=['filename'], varargs=None, varkw=None, defaults=None, kwonlyargs=[], kwonlydefaults=None, annotations={'return': None, 'filename': <class 'str'>})
