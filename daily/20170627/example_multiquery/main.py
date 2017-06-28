import mongomock
import prepare
import sqlite3


def get_mongo_client():
    return mongomock.MongoClient()


def get_sqlite_connection():
    connection = sqlite3.connect(":memory:")
    connection.row_factory = lambda c, row: {col[0]: row[i] for i, col in enumerate(c.description)}
    return connection


def main():
    import logging
    logging.basicConfig(level=logging.DEBUG)

    client = get_mongo_client()
    connection = get_sqlite_connection()
    prepare.init_mongo(client)
    prepare.init_sql(connection)
    run(client, connection)
    connection.close()


def run(client, connection):
    from lib import ResourceFactory
    rf = ResourceFactory()
    db = client["test"]

    qs = rf.mongo(db.users)
    qs = qs.bind_one("group", rf.sql(connection, "groups"), "u.group_id==g.id")
    qs = qs.bind_many("skills", rf.mongo(db.skills), "u._id==s.user_id")

    import json
    for row in qs:
        print(json.dumps(row, indent=2, default=lambda x: dict(x) if hasattr(x, "get") else str(x)))


if __name__ == "__main__":
    import logging
    logging.basicConfig(level=logging.DEBUG)
    main()
