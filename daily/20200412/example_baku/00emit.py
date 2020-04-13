from baku.codegen.detect import detect
from baku.codegen.emit import emit
from handofcats import as_command
from dictknife import loading


@as_command
def run(filename: str) -> None:
    data = loading.loadfile(filename)
    result = detect(data)
    print(emit(result))
