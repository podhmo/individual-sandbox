from pprint import pprint
from handofcats import as_command
from sqlalchemy import create_engine, MetaData


@as_command
def run(*, db: str) -> None:
    engine = create_engine(db)
    metadata = MetaData(bind=engine)
    metadata.reflect()
    # print(metadata)
    # print(dir(metadata))
    for t in metadata.sorted_tables:
        print(f"## {t}")
        print("")
        print("```")
        pprint(vars(t))
        print("```")
        print("")
