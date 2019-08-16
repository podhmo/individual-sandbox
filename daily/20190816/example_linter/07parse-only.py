import subprocess
import pathlib
import os.path
import sys
import logging
from dictknife.jsonknife import Expander, get_resolver
from dictknife import loading
from yaml.error import MarkedYAMLError

logger = logging.getLogger(__name__)


def main():
    filename = sys.argv[1]
    resolver = get_resolver(filename)
    try:
        expander = Expander(resolver)
        doc = expander.expand()
        loading.dumpfile(doc)
    except MarkedYAMLError as e:
        padding = ""
        mark = e.context_mark or e.problem_mark
        filename = mark.name
        for r in resolver.path_list(filename):
            padding += "  "
            print(padding, os.path.relpath(r.filename))

        padding += "  "
        print(padding, "problem", e.problem, "@", e.problem_mark)
        print(padding, "context", e.context, "@", e.context_mark)
        print("")
    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    main()
