from handofcats import as_command
from miniconfig import Configurator as _Configurator


class Configurator(_Configurator):
    pass


@as_command
def run():
    c = Configurator()
    print("setup")
    c.include("plugins.foo")
    c.include("plugins.foo")
    print("before commit")
    c.commit()
    print("after commit")
