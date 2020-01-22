import typing as t
import sys
import site
import pathlib
import runpy


def get_pyi_files(
    typeshed_path: pathlib.Path,
    name: str,
    *,
    package="stdlib",
    version_info=sys.version_info[:2],
) -> t.Iterable[pathlib.Path]:
    candidates = [
        f"{version_info[0]}.{version_info[1]}",
        f"{version_info[0]}",
        f"2and3",
    ]
    for suffix in candidates:
        p = typeshed_path.joinpath(package, suffix, name)
        if p.exists():
            yield p


for d in site.getsitepackages():
    typeshed_path = pathlib.Path(d).joinpath("mypy/typeshed")
    if not typeshed_path.exists():
        continue

    for f in get_pyi_files(typeshed_path, "functools.pyi"):
        m = runpy.run_path(str(f))
        print(m)
