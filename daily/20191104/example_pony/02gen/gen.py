from sqlalchemy import create_engine, MetaData
from handofcats import as_command
from dictknife import loading


@as_command
def run(*, db: str) -> None:
    engine = create_engine(db)
    metadata = MetaData(bind=engine)
    metadata.reflect()

    defs = {}
    for t in metadata.sorted_tables:
        if str(t.name) == "sqlite_sequence":
            continue

        defs[str(t.name)] = prop = {}
        for col in t.columns:
            type_name = col.type.__visit_name__
            if type_name == "null":
                type_name = None
            prop[str(col.name)] = {"type": type_name, "primary_key": col.primary_key}
    loading.dumpfile(defs)
