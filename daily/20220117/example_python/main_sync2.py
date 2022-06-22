from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import create_engine
from sqlalchemy import MetaData
from sqlalchemy import select
from sqlalchemy.orm import Session
from sqlalchemy.orm import registry


class User:
    __table_args__ = {"autoload": True}
    id = Column(Integer, primary_key=True)


def main():
    engine = create_engine(
        "sqlite:///data.db",
        echo=True,
    )
    mapper_registry = registry()
    metadata = MetaData()
    metadata.reflect(bind=engine)

    mapper_registry.map_imperatively(User, metadata.tables["User"])

    with Session(engine) as session:
        result = session.execute(select(User).order_by(User.id))
        for x in result.scalars():
            print(x, x.id, x.name)  # nameがautoloadにより付加


main()
