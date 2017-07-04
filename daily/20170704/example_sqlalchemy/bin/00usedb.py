import os
import sqlalchemy as sa
from handofcats import as_command


@as_command
def main(src="../src/groups.db", echo=False):
    if "://" not in src:
        src = "sqlite:///{}".format(os.path.join(os.getcwd(), src))
    engine = sa.create_engine(src, echo=echo)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect(engine)
    print("----------------------------------------")

    users = metadata.tables["users"]

    for row in users.select().where(users.c.id == 1).execute():
        print(type(row), row, dict(row))
    for row in users.select().where(users.c.id == 1).with_only_columns([users.c.id]).execute():
        print(type(row), row, dict(row))
