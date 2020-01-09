from monogusa.web.codegen import _codeobject as codeobject
from monogusa.web.codegen._codeobject import Module
from monogusa.cli import runtime


m = Module()


class pool:
    pass


argparse = m.import_("argparse")
pool.parser = argparse.ArgumentParser()
driver = runtime.Driver(parser=m.let("parser", argparse.ArgumentParser()))
