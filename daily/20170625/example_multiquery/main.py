import mongomock
import prepare


def patch(Cursor):
    # monkey patch
    original = Cursor._refresh

    def _refresh(self):
        print("hmm")
        return original(self)

    _refresh.marked = True
    if not getattr(Cursor._refresh, "marked", False):
        Cursor._refresh = _refresh


def get_client():
    return mongomock.MongoClient()


def main():
    import logging
    logging.basicConfig(level=logging.DEBUG)

    client = get_client()
    prepare.init(client)
    run(client)


def run(client):
    from lib import ResourceFactory
    rf = ResourceFactory()

    db = client["test"]
    qs = (
        rf.mongo(db.users).bind_one("group", rf.mongo(db.groups), "u.group_id==g._id")
        .bind_many("skills", rf.mongo(db.skills), "u._id==s.user_id")
    )

    import json
    for row in qs:
        print(json.dumps(row, indent=2, default=lambda x: dict(x) if hasattr(x, "get") else str(x)))


def run_by_hand(client):
    from collections import defaultdict
    from dictknife import pp

    db = client["test"]

    # join by hand

    users = list(db.users.find({}))
    skills = db.skills.find({"user_id": {"$in": list({u["_id"] for u in users})}})
    skill_map = defaultdict(list)
    for s in skills:
        skill_map[s["user_id"]].append(s)

    groups = db.groups.find({"_id": {"$in": list({u["group_id"] for u in users})}})
    group_map = {s["_id"]: s for s in groups}

    tris = []
    for u in users:
        tris.append(dict(user=u, group=group_map[u["group_id"]], skills=skill_map[u["_id"]]))

    pp(tris)


if __name__ == "__main__":
    main()
