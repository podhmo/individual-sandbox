import typing as t
import sys
import logging


class Driver:
    format = "%(levelname)s\t%(asctime)s\t%(name)s\t%(message)s\tin\t%(filename)s:%(lineno)s\t%(funcName)s"
    out = sys.stderr
    level = logging.DEBUG

    def get_logger(self, name):
        return logging.getLogger(name)

    def setup(
        self,
        *,
        level: t.Optional[int] = None,
        out: t.Optional[t.IO] = None,
        format: t.Optional[str] = None
    ):
        logging.basicConfig(
            level=level or self.level,
            format=format or self.format,
            stream=out or self.out,
        )
