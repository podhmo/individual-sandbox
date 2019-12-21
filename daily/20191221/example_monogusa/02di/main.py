import inspect
from monogusa.dependencies import scan_module
from monogusa.web.codegen._fnspec import fnspec
from prestring.python import Module
from prestring.utils import LazyArguments
import commands as target_module


m = Module()
m.from_("fastapi", "Depends")
m.sep()

for component in scan_module(target_module).components:
    spec = fnspec(component)
    if inspect.iscoroutinefunction(spec.body):
        raise NotImplementedError("async support")
    else:
        args = []
        for name, val, kind in spec.arguments:
            args.append(f"{name}: {spec.type_str_of(val)} = Depends({name})")
        with m.def_(spec.name, LazyArguments(args), return_type=spec.return_type):
            m.return_(
                "{}({})",
                spec.fullname,
                LazyArguments([name for name, _, _ in spec.arguments]),
            )
    print(spec.name, LazyArguments([name for name, _, _, in spec.arguments]))
print(m)
