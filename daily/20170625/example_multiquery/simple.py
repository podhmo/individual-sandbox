import logging
import mongomock
import prepare


def get_client():
    return mongomock.MongoClient()


def main():
    client = get_client()
    prepare.init(client)
    run(client)


def run(client):
    from lib import Resource

    db = client["test"]
    qs = users = Resource(db.users)
    qs = qs.bind_one(
        "group",
        Resource(db.groups).in_("_id", [u["group_id"] for u in users]),
        "u.group_id==g._id",
    )
    qs = qs.bind_many(
        "skills",
        Resource(db.skills).in_("user_id", [u["_id"] for u in users]),
        "u._id==s.user_id",
    )
    import json
    for row in qs:
        print(json.dumps(row, indent=2, default=lambda x: dict(x) if hasattr(x, "get") else str(x)))


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    main()
