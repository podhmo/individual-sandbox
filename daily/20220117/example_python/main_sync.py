from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import create_engine
from sqlalchemy import select
from sqlalchemy.orm import Session
from sqlalchemy.orm import declarative_base


engine = create_engine(
    "sqlite:///data.db",
    echo=False,
)
Base = declarative_base()
Base.metadata.bind = engine


class User(Base):
    __tablename__ = "user"
    __table_args__ = {"autoload": True}
    id = Column(Integer, primary_key=True)


def main():
    with Session(engine) as session:
        result = session.execute(select(User).order_by(User.id))
        for x in result.scalars():
            print(x, x.id, x.name)  # nameがautoloadにより付加


main()
