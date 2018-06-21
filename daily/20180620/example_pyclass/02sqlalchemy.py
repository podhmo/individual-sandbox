from shape import parse, emit
from sqlalchemy import Column, String, Integer
from sqlalchemy.ext import declarative

Base = declarative.declarative_base()


class User(Base):
    __tablename__ = 'users'
    id = Column(Integer, primary_key=True)
    name = Column(String(64), nullable=False)


print(emit(parse(Column)))
