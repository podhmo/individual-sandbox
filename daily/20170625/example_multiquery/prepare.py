import bson


def init(client):
    client.drop_database("test")
    db = client["test"]

    groups = [
        {
            "_id": bson.ObjectId(),
            "name": "A"
        },
        {
            "_id": bson.ObjectId(),
            "name": "B"
        },
    ]
    users = [
        {
            "_id": bson.ObjectId(),
            "group_id": groups[0]["_id"],
            "name": "foo"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": groups[0]["_id"],
            "name": "bar"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": groups[0]["_id"],
            "name": "baz"
        },
        {
            "_id": bson.ObjectId(),
            "group_id": groups[1]["_id"],
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
    db.groups.insert_many(groups)
    db.users.insert_many(users)
    db.skills.insert_many(skills)
