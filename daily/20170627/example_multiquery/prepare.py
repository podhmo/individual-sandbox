import bson


def init_sql(connection):
    c = connection.cursor()
    c.execute(
        """
        CREATE TABLE groups
        (id integer not null unique, name text not null);
        """
    )
    groups = [
        (1, "A"),
        (2, "B"),
    ]
    c.executemany("INSERT INTO groups VALUES (?, ?)", groups)


def init_mongo(client):
    client.drop_database("test")
    db = client["test"]
    users = [
        {
            "_id": bson.ObjectId(),
            "group_id": 1,
            "name": "foo"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": 1,
            "name": "bar"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": 1,
            "name": "baz"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": 2,
            "name": "boo"
        },
    ]
    skills = [
        {
            "_id": bson.ObjectId(),
            "user_id": users[0]["_id"],
            "name": "X"
        },
        {
            "_id": bson.ObjectId(),
            "user_id": users[1]["_id"],
            "name": "X"
        },
        {
            "_id": bson.ObjectId(),
            "user_id": users[2]["_id"],
            "name": "X"
        },
        {
            "_id": bson.ObjectId(),
            "user_id": users[1]["_id"],
            "name": "Y"
        },
        {
            "_id": bson.ObjectId(),
            "user_id": users[2]["_id"],
            "name": "Y"
        },
        {
            "_id": bson.ObjectId(),
            "user_id": users[2]["_id"],
            "name": "Z"
        },
    ]
    db.users.insert_many(users)
    db.skills.insert_many(skills)
