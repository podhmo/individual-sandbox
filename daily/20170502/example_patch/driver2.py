import sys
from zenmai.driver import Driver as _Driver
from zenmai.core.context import Scope


class Driver(_Driver):
    def context_factory(self, module, *args, **kwargs):
        scope = Scope.mergewith(module, parent=sys.modules["builtins"])
        return super().context_factory(scope, *args, **kwargs)
