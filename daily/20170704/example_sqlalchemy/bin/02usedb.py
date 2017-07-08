import sqlalchemy as sa


def run(url, *, echo):
    config = {"url": url}
    engine = sa.engine_from_config(config, prefix="")
    metadata = sa.MetaData(bind=engine)
    metadata.reflect(engine)
    engine.echo = echo

    with engine.connect() as conn:
        usedb(conn, metadata.tables)


def usedb(conn, tables):
    for row in conn.execute(tables["users"].select()):
        print(row)

    qs = tables["users"].join(tables["skills"], tables["users"].c.id == tables["skills"].c.user_id)
    for row in conn.execute(qs.select().where(tables["users"].c.id == 1)):
        print(row)

    qs = tables["users"].join(tables["skills"], tables["users"].c.id == tables["skills"].c.user_id)
    for rows in chunked(conn.execute(qs.select().where(tables["users"].c.id == 1)), n=2):
        print(rows)

    qs = tables["users"].join(tables["skills"], tables["users"].c.id == tables["skills"].c.user_id)
    for row in conn.execute(
        sa.sql.select([tables["users"].c.name, tables["skills"].c.name]).select_from(qs).where(tables["users"].c.id == 1)
    ):
        print(row)


def chunked(cursor, *, n):
    while True:
        rows = cursor.fetchmany(n)
        if not rows:
            break
        yield rows


url = "sqlite:///../src/groups.db"
run(url, echo=True)
