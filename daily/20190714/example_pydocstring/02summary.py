import math
import dataclasses
from handofcats import as_command
from dictknife import loading


@dataclasses.dataclass
class Summary:
    name: str
    object_count: int = 0
    empty_count: int = 0
    docstring_count: int = 0
    total_length: int = 0
    min_length: int = math.inf  # xxx: int?
    max_length: int = 0
    average_length: int = 0


@as_command
def run(filename: str) -> None:
    d = loading.loadfile(filename)
    summaries = []
    for modname, members in d.items():
        s = Summary(name=modname)
        summaries.append(s)
        for name, docstring in members.items():
            s.object_count += 1
            if docstring is None:
                s.empty_count += 1
            else:
                s.docstring_count += 1
                length = len(docstring.strip().split("\n"))
                s.total_length += length
                s.min_length = min(s.min_length, length)
                s.max_length = max(s.max_length, length)
        if s.docstring_count:
            s.average_length = s.total_length / s.docstring_count

    loading.dumpfile([s.__dict__ for s in summaries], format="tsv")
