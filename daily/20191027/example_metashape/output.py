import logging
import os.path
import glob
import dataclasses

logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=False, unsafe_hash=False)
class Option:
    root: str
    files: dict = dataclasses.field(default_factory=dict)
    prefix: str = "autogen_"
    suffix: str = ".py"


class File:
    def __init__(self, name: str, *, consume, content):
        self.name = name
        self.content = content
        self.consume = consume

    def __enter__(self):
        return self.content

    def __exit__(self, typ, val, tb):
        pass

    def write(self, wf):
        self.consume(wf, self.content)


class PrestringResolver:
    def __init__(self, *, module_factory):
        self.module_factory = module_factory

    def resolve(self, name):
        return File(name, content=self.module_factory(), consume=self.consume)

    def consume(self, wf, content):
        wf.write(str(content))


# TODO:
# separate class
# option
def cleanup_pyfile(option: Option):
    # TODO: recursive?
    for f in glob.glob(os.path.join(option.root, f"{option.prefix}*{option.suffix}")):
        logger.info("remove file path=%s", f)
        os.remove(f)


class Writer:
    def __init__(self, option: Option):
        self.option = option

    def write_all(self):
        for name, f in self.option.files.items():
            self.write(name, f)

    def write(self, name, file, *, _retry=False):
        dirname, basename = os.path.split(name)
        fname = "{}{}".format(self.option.prefix, basename)
        fullpath = os.path.join(self.option.root, os.path.join(dirname, fname))

        logger.info("touch file path=%s", fullpath)

        try:
            with open(fullpath, "w") as wf:
                file.write(wf)
        except FileNotFoundError:
            if _retry:
                raise
            logger.info("touch directory path=%s", os.fullpath.dirname(fullpath))
            os.makedirs(os.fullpath.dirname(fullpath), exist_ok=True)
            return self.write(file, _retry=True)


class FS:
    def __init__(self, option: Option, *, cleanup=cleanup_pyfile):
        self.option = option
        self.cleanup = cleanup

    def file(self, f):
        self.option.files[f.name] = f  # xxx:
        return f

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        if self.cleanup is not None:
            self.cleanup(self.option)
        w = Writer(self.option)
        w.write_all()
