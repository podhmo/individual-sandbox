import os
import sqlalchemy as sa
from handofcats import as_command


def get_table(metadata, name):
    return metadata.tables[name]


def run(url, *, echo):
    config = {"url": url, "echo": echo}
    engine = sa.engine_from_config(config, prefix="")
    metadata = sa.MetaData(bind=engine)
    metadata.reflect(engine)
    engine.echo = True
    users = get_table(metadata, "users")

    with engine.connect() as conn:
        for row in conn.execute(sa.sql.select([users.c.id]).where(users.c.id == 1)):
            print(type(row), row, dict(row))


@as_command
def main(src="../src/groups.db", echo=False):
    if "://" not in src:
        src = "sqlite:///{}".format(os.path.join(os.getcwd(), src))
    return run(src, echo=echo)
