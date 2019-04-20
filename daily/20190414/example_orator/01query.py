from orator import DatabaseManager
import logging

logging.basicConfig(level=logging.DEBUG)

# https://orator-orm.com/docs/0.9/query_builder.html
# https://orator-orm.com/docs/0.9/basic_usage.html#query-logging
config = {
    "sqlite3": {"driver": "sqlite", "database": "examples.db", "log_queries": True}
}

db = DatabaseManager(config)

with db.transaction():
    user = db.table("users").first()
    print(user)

with db.transaction():
    user = db.table("users").select("id", "name").first()
    print(user)

print("----------------------------------------")
for users in db.table("users").select("id", "name").chunk(100):
    for user in users:
        print(user)
