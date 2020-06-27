import pathlib
from aiosql import query_loader
from aiosql.aiosql import _make_driver_adapter
from aiosql.types import SQLOperationType
from parse import parse
from json import dumps

ql = query_loader.QueryLoader(_make_driver_adapter("sqlite3"), record_classes=None)
for qdatum in ql.load_query_data_from_file(pathlib.Path("./queries.sql")):
    # print(qdatum)
    print("-")
    if qdatum.operation_type in (SQLOperationType.SELECT,  SQLOperationType.SELECT_ONE):
        print(qdatum.sql)
        print(dumps(parse(qdatum.sql), indent=2, ensure_ascii=False))
