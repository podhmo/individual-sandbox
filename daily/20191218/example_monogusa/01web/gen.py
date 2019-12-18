from types import ModuleType
from magicalimport import import_module
from prestring.python import Module
from prestring.naming import pascalcase
import commands

fnspec = import_module("../fnspec.py", here=__file__)


def gen(target: ModuleType, *, m=None):
    m = m or Module()

    m.import_("typing as t")
    m.from_("pydantic", "BaseModel")
    m.from_("fastapi", "APIRouter", "Depends")
    m.from_("monogusa.web", "runtime")
    m.import_(target.__name__)
    m.stmt("router = APIRouter()")

    m.sep()

    # TODO: collect functions and use them.
    spec = fnspec.fnspec(commands.hello)
    co = fnspec.spec_to_schema_code(spec, name=pascalcase(spec.name))
    co.emit(m=m)

    m.stmt('@router.post ( "/{}", response_model=runtime.CommandOutput )', spec.name)
    with m.def_(
        spec.name,
        f"input: {co.name}",
        "writer: commands.Writer =  Depends(commands.writer)",  # deps
        return_type="t.Dict[str, t.Any]",
    ):
        with m.with_("runtime.handle() as s"):
            m.stmt("{}.{}(writer, **input.dict())", target.__name__, spec.name)
            m.stmt("return s.dict()")

    return m


print(gen(commands))
