from typing import Optional, Any
from types import ModuleType
from importlib.machinery import ModuleSpec


class PrestringImporter:
    @classmethod
    def find_spec(
        cls, fullname: str, path: Optional[str] = None, target: Optional[Any] = None
    ) -> ModuleSpec:
        import os.path

        if not os.path.exists(f"{fullname}.pym"):
            return None
        loader = cls
        return ModuleSpec(fullname, loader)

    @classmethod
    def create_module(cls, spec: ModuleSpec) -> Optional[ModuleType]:
        return None

    @classmethod
    def exec_module(cls, module: ModuleType) -> Any:
        with open(f"{module.__name__}.pym") as rf:
            code = rf.read()
            code = "\n".join(
                [
                    "from prestring.python import Module",
                    "m = Module()",
                    code,
                    "print(m)",
                ]
            )
        exec(code, module.__dict__)

