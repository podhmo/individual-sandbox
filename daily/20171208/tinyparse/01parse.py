from mypy.parse import parse
from mypy.main import process_options


def run(filepath):
    with open(filepath) as rf:
        source = rf.read()

    sources, options = process_options([filepath])
    return parse(source, filepath, None, options)


t = run("./src/01.py")
print(t)
# run("./src/01.py")
