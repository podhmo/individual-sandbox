import sqlalchemy as sa
from sqlalchemy.ext.declarative import declared_attr, declarative_base
import sqlalchemy.orm as orm

Base = declarative_base()


class IdMixin:
    @declared_attr
    def id(cls):
        for base in cls.__mro__[1:-1]:
            if getattr(base, '__table__', None) is not None:
                type = sa.ForeignKey(base.id)
                break
        else:
            type = sa.Integer

        return sa.Column(type, primary_key=True)


class Group(IdMixin, Base):
    __tablename__ = "Group"

    name = sa.Column(sa.String(255), default="", nullable=False)


class User(IdMixin, Base):
    __tablename__ = "User"

    name = sa.Column(sa.String(255), default="", nullable=True)
    group_id = sa.Column(sa.Integer, sa.ForeignKey(Group.id), nullable=False)
    group = orm.relationship(Group, uselist=False, backref="users")
