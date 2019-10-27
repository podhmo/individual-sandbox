import logging
import os.path
import glob
from collections import defaultdict


logger = logging.getLogger(__name__)


class File:
    name: str
    m: object  # xxx

    def __init__(self, name: str, m, *, fs):
        self.name = name
        self.m = m
        self._fs = fs

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        self._fs.output_file(self)


# TODO:
# cleanup option
# context manager
# separate class
# option


class SeparatedOutput:
    @staticmethod
    def module_factory():
        raise NotImplementedError("e.g. python.PythonModule()")

    def __init__(self, dirname, prefix="autogen_", module_factory=None):
        self.dirname = dirname
        self.prefix = prefix
        self.arrived = set()
        self.files = {}
        self.module_factory = module_factory or self.__class__.module_factory

    def open(self, file_name, mode, m=None):
        dirname, basename = os.path.split(file_name)
        fname = "{}{}".format(self.prefix, basename)
        m = m or self.module_factory()
        f = File(os.path.join(dirname, fname), m=m, fs=self)
        self.files[f.name] = f
        return f

    def prepare(self, f):
        dirname = os.path.dirname(os.path.join(self.dirname, f.name))
        if dirname in self.arrived:
            return
        self.arrived.add(dirname)
        logger.info("touch directory path=%s", dirname)
        os.makedirs(dirname, exist_ok=True)
        if self.prefix:
            for f in glob.glob(os.path.join(dirname, "{}*.py".format(self.prefix))):
                os.remove(f)

    def output(self):
        for file in self.files.values():
            self.output_file(file)

    def output_file(self, file):
        self.prepare(file)

        path = os.path.join(self.dirname, file.name)
        logger.info("touch file path=%s", path)
        with open(path, "w") as wf:
            wf.write(str(file.m))

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        self.output()
