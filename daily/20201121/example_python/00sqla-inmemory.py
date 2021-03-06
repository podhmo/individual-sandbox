import sqlalchemy as sa
import typing_extensions as tx
from handofcats import as_command

metadata = sa.MetaData()

notes = sa.Table(
    "notes",
    metadata,
    sa.Column("id", sa.Integer, primary_key=True),
    sa.Column("text", sa.String),
    sa.Column("completed", sa.Boolean),
)


@as_command()
def run(*, format: tx.Literal["json", "tabular"] = "tabular") -> None:
    db_url = "sqlite:///:memory:"
    engine = sa.create_engine(
        db_url, connect_args={"check_same_thread": False}, echo=True  # , future=True
    )
    metadata.create_all(bind=engine)

    # print(notes.insert().values(text="foo", completed=False).compile())
    with engine.begin() as conn:
        conn.execute(
            notes.insert(),
            [
                {"text": "foo", "completed": False},
                {"text": "bar", "completed": False},
                {
                    "text": "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
                    "completed": False,
                },
                {"text": "boo", "completed": False},
            ],
        )

        # need transaction?
        # with conn.begin():
        # legacy conn.execute(sa.select([notes]))
        rows = conn.execute(notes.select())

        if format == "tabular":
            from rich.console import Console
            from rich.table import Table

            table = Table(title="notes")

            table.add_column("id", justify="right")
            table.add_column("text", justify="left", width=80)
            table.add_column("completed", justify="right")

            for row in rows:
                table.add_row(str(row.id), row.text, str(row.completed))

            console = Console()
            console.print(table)
        else:
            import json

            for row in rows:
                print(json.dumps(dict(row)))
