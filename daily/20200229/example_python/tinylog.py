import os
import sys


def get_print_function(*, _depth=1, name=None, _ignored=[__file__]):
    if bool(os.environ.get("DEBUG", "").strip()):
        import logging

        if name is None:
            import inspect

            s = inspect.stack(_depth)
            for frame_info in s:
                filename = inspect.getfile(frame_info.frame)
                if filename.startswith("<frozen importlib"):
                    continue
                if filename in _ignored:
                    continue
                break

            name = frame_info.frame.f_globals.get("__name__")
        return logging.getLogger(name).info
    else:
        return sys.modules["builtins"].print


print = get_print_function()
__all__ = ["print"]
