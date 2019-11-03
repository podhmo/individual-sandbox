import dataclasses
import pathlib
import logging
from handofcats import as_command
from prestring.output import output


@dataclasses.dataclass
class Config:
    name: str = "foo"
    version: str = "0.0.0"
    description: str = dataclasses.field(
        default="", metadata=dict(description="help usage")
    )


@as_command  # type: ignore
def run(dst:str) -> None:
    c = Config()
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
