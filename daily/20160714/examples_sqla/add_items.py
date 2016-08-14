# -*- coding:utf-8 -*-

import sqlalchemy as sa
import sqlalchemy.orm as orm
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import scoped_session, sessionmaker

Session = scoped_session(sessionmaker())
engine = sa.create_engine('sqlite://', echo=False)
Session.configure(bind=engine)

Base = declarative_base(bind=engine)


class User(Base):
    __tablename__ = "User"

    id = sa.Column(sa.Integer, primary_key=True)
    group_id = sa.Column(sa.Integer, sa.ForeignKey("Group.id"))
    group = orm.relationship("Group", backref="users", uselist=False)
    name = sa.Column(sa.String(255), default="", nullable=False)


class Group(Base):
    __tablename__ = "Group"
    id = sa.Column(sa.Integer, primary_key=True)
    name = sa.Column(sa.String(255), default="", nullable=False)


Base.metadata.create_all()
g = Group(name="A")
g.users.append(User(name="x"))
g.users.append(User(name="y"))
g.users.append(User(name="z"))
Session.add(g)
Session.commit()

qs = Session.query(Group)

print("--")
print("sql: '{}', engine: '{}'".format(qs.statement, qs.session.bind))

g = qs.one()
qs = Session.query(User).filter(User.group_id == g.id)
print("sql: '{}', engine: '{}'".format(qs.statement, qs.session.bind))
