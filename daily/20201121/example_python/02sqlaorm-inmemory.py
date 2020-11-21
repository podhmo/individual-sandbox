import sqlalchemy as sa
from sqlalchemy.orm import declarative_base
from sqlalchemy.orm import sessionmaker
import typing_extensions as tx
from handofcats import as_command

metadata = sa.MetaData()
Base = declarative_base(metadata=metadata)
Session = sessionmaker(autoflush=False, autocommit=False)


class Note(Base):
    __tablename__ = "notes"

    id = sa.Column(sa.Integer, primary_key=True)
    text = sa.Column(sa.String, nullable=False)
    completed = sa.Column(sa.Boolean, nullable=False, default=False)


@as_command()
def run(*, format: tx.Literal["json", "tabular"] = "tabular") -> None:
    db_url = "sqlite:///:memory:"
    engine = sa.create_engine(
        db_url, connect_args={"check_same_thread": False}, echo=True  # , future=True
    )
    metadata.create_all(bind=engine)
    session = Session(bind=engine)

    with session.begin():
        # or session.add(..)

        session.bulk_save_objects(
            [Note(text="foo"), Note(text="bar"), Note(text="boo")]
        )

    with session.begin():
        rows = session.query(Note).all()
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

            # more detail: https://stackoverflow.com/questions/5022066/how-to-serialize-sqlalchemy-result-to-json
            for row in rows:
                print(
                    json.dumps(
                        {c.name: getattr(row, c.name) for c in row.__table__.columns}
                    )
                )
