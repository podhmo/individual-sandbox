import pathlib
from aiosql import query_loader
from aiosql.aiosql import _make_driver_adapter
from aiosql.types import SQLOperationType, QueryDatum
from prestring.python.codeobject import Module
from parse import parse
from json import dumps


def make_code(m: Module, qd: QueryDatum) -> str:
    name = qd.query_name

    args = []
    seen = set()

    def visit(d):
        if hasattr(d, "keys"):
            for k, v in d.items():
                visit(k)
                visit(v)
        elif hasattr(d, "append"):
            for v in d:
                visit(v)
        elif hasattr(d, "startswith"):
            if d.startswith(":"):
                if d in seen:
                    return
                seen.add(d)
                args.append(d[1:])

    d = parse(qdatum.sql)
    visit(d)

    with m.def_(qd.query_name, *args):
        m.docstring(qd.doc_comments)
        m.stmt("do('''{}''')", qd.sql.rstrip("\n;"))
        m.stmt("data = {}", dumps(d, indent=2))


ql = query_loader.QueryLoader(_make_driver_adapter("sqlite3"), record_classes=None)

m = Module()
for qdatum in ql.load_query_data_from_file(pathlib.Path("./queries.sql")):
    # print(qdatum)
    if qdatum.operation_type in (SQLOperationType.SELECT, SQLOperationType.SELECT_ONE):
        make_code(m, qdatum)
print(m)
