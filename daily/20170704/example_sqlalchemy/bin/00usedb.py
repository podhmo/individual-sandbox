import os
from handofcats import as_command
from sqlalchemy import create_engine, MetaData


@as_command
def main(src, echo=False):
    if "://" not in src:
        src = "sqlite:///{}".format(os.path.join(os.getcwd(), src))
    engine = create_engine(src, echo=echo)
    metadata = MetaData(bind=engine)
    metadata.reflect(engine)
    print("----------------------------------------")

    users = metadata.tables["users"]

    for row in users.select().where(users.c.id == 1).execute():
        print(type(row), row, dict(row))
    for row in users.select().where(users.c.id == 1).with_only_columns([users.c.id]).execute():
        print(type(row), row, dict(row))
