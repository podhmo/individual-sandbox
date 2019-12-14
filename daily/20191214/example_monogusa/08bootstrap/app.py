import sqlalchemy
from magicalimport import import_symbol
from sqlalchemy.engine.base import Engine
from sqlalchemy import log

# Database table definitions.
DATABASE_URL = "sqlite:///test.db"
metadata = sqlalchemy.MetaData()


notes = sqlalchemy.Table(
    "notes",
    metadata,
    sqlalchemy.Column("id", sqlalchemy.Integer, primary_key=True),
    sqlalchemy.Column("text", sqlalchemy.String),
    sqlalchemy.Column("completed", sqlalchemy.Boolean),
)


fixture = import_symbol("bootstrap.py:fixture", here=__file__)


@fixture
def engine():
    # TODO: teardown
    return sqlalchemy.create_engine(
        DATABASE_URL, connect_args={"check_same_thread": False}
    )


def init_db(engine: Engine, *, debug: bool, message: str = "hmm"):
    """init tables"""
    if debug:
        log.instance_logger(engine, echoflag=True)
    metadata.create_all(bind=engine)
    engine.dispose()
    print(f"** init_db {debug=} {message=} **")


resolve_args = import_symbol("bootstrap.py:resolve_args", here=__file__)
args = resolve_args(init_db)
init_db(*args, debug=True)
