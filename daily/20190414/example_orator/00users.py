from orator import Model, SoftDeletes
from orator import DatabaseManager
from orator.migrations import Migration
import logging

logging.basicConfig(level=logging.DEBUG)


class User(SoftDeletes, Model):
    __fillable__ = ["name", "email"]


class CreateTableUsers(Migration):
    def up(self):
        pass

    def down(self):
        pass


# https://orator-orm.com/docs/0.9/basic_usage.html#query-logging
config = {"sqlite3": {"driver": "sqlite", "database": ":memory:", "log_queries": True}}

db = DatabaseManager(config)
Model.set_connection_resolver(db)

with db.transaction():
    User.create(name="John")
