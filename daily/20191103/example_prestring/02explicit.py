import sys
import pathlib
import logging
import pydantic
from pydantic import dataclasses
from dictknife import loading
from handofcats import as_command
from prestring.output import output


@dataclasses.dataclass
class Config:
    name: str = "foo"
    version: str = "0.0.0"
    description: str = ""


@as_command  # type: ignore
def run(dst: str, *, config: str) -> None:
    d = loading.loadfile(config)
    try:
        c = Config(**d)
    except pydantic.ValidationError as e:
        print(e.json(), file=sys.stderr)
        sys.exit(1)

    logging.basicConfig(level=logging.DEBUG)

    # NOCHECK=1 FAKE=1 VERBOSE=1
    with output(dst) as fs:
        project_dir = pathlib.Path(c.name)

        with fs.open(project_dir / ".gitignore", "w") as wf:
            print("*.py", file=wf)

        with fs.open(project_dir / "README.md", "w") as wf:
            print(f"# {c.name}", file=wf)
            print("", file=wf)
            print(c.description, file=wf)

        fs.open(project_dir / "CHANGES.rst", "w")
