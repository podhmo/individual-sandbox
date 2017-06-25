import mongomock

# see also: https://github.com/mongomock/mongomock/blob/develop/tests/test__bulk_operations.py


def get_client():
    return mongomock.MongoClient()


def main():
    client = get_client()
    db = client["me"]

    db.people.drop()
    db.people.create_index("gender", unique=False, name="idx_gender", sparse=True, background=True)
    run(db)


def run(db):

    print("pre insert count: ", db.people.count())
    db.people.insert_one({"name": "foo", "age": 20, "gender": "F"})
    db.people.insert_one({"name": "bar", "age": 20, "gender": "F"})
    db.people.insert_one({"name": "boo", "age": 21, "gender": "M"})
    print("post insert count: ", db.people.count())

    print("query")
    for row in db.people.find({"gender": "F"}):
        print("\t", row)

    print("update")
    print(
        "\t",
        db.people.update_one({
            "gender": "F"
        }, {"$set": {
            "_updated": True
        },
            "$inc": {
                "age": 1
            }})
    )
    print(
        "\t",
        db.people.update_many({
            "gender": "F"
        }, {"$set": {
            "_updated": True
        },
            "$inc": {
                "age": 1
            }})
    )

    print("query")
    for row in db.people.find({"gender": "F"}):
        print("\t", row)

    print("pre bulk insert count: ", db.people.count())
    db.people.insert_many(
        [
            {
                "name": "X",
                "age": 0
            },
            {
                "name": "Y",
                "age": 0
            },
            {
                "name": "Z",
                "age": 0
            },
        ]
    )
    print("post bulk insert count: ", db.people.count())


if __name__ == "__main__":
    main()
