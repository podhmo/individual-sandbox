from handofcats.actions._codeobject import Module
from prestring.utils import LazyArguments

m = Module()
argparse = m.import_("argparse")
# argparse.ArgumentParser(
# )
cls = m.symbol("f")(
    (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter,),
)
print(cls)

