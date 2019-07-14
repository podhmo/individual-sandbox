import inspect
from magicalimport import import_module
from handofcats import as_command


def get_summary(doc):
    if not doc:
        return doc
    return doc.split("\n\n", 1)[0]


@as_command
def run(pkg: str) -> None:
    m = import_module(pkg)
    for name, v in m.__dict__.items():
        if inspect.isfunction(v) or inspect.isclass(v):
            if v.__module__ == m.__name__:
                print(f"{name}	{get_summary(inspect.getdoc(v) or '')}")
