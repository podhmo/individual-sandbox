import logging
import sqlalchemy

# https://www.encode.io/databases/database_queries/

metadata = sqlalchemy.MetaData()
logger = logging.getLogger(__name__)

notes = sqlalchemy.Table(
    "notes",
    metadata,
    sqlalchemy.Column("id", sqlalchemy.Integer, primary_key=True),
    sqlalchemy.Column("text", sqlalchemy.String(length=100), nullable=False),
    sqlalchemy.Column("completed", sqlalchemy.Boolean, nullable=False),
)


def main() -> None:
    # Create a table.
    engine = sqlalchemy.create_engine("sqlite:///:memory:", echo=True)
    metadata.create_all(bind=engine)

    with engine.connect() as conn:
        # Delete a table.
        query = notes.delete()
        conn.execute(query)

        # Execute
        query = notes.insert()
        values = {"text": "example1", "completed": True}
        conn.execute(query.values(**values))

        # Execute many
        query = notes.insert()
        values = [
            {"text": "example2", "completed": False},
            {"text": "example3", "completed": True},
        ]
        conn.execute(query, values)

        query = notes.select()
        rows = conn.execute(query)
        print("# Fetch multiple rows")
        print(rows.fetchall())

        query = notes.select()
        rows = conn.execute(query)
        print("# Fetch single row")
        print(rows.first())


# logging.basicConfig(level=logging.DEBUG)
logging.basicConfig(level=logging.INFO)

# NOTE: echo flag, logging messages are duplicated
logging.getLogger("sqlalchemy").propagate = False

main()
