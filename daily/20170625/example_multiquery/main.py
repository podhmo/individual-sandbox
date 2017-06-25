import mongomock
import prepare


def get_client():
    return mongomock.MongoClient()


def main():
    client = get_client()
    prepare.init(client)
    run(client)


def run(client):
    from lib import MultiResource, Resource

    db = client["test"]
    mr = MultiResource(dict(u=Resource(db.users), g=Resource(db.groups), s=Resource(db.skills)))
    mr = mr.restrict("g", lambda g: g.in_("_id", [u["group_id"] for u in mr.u])).many_to_one("u.group_id", "g._id")
    mr = mr.restrict("s", lambda s: s.in_("user_id", [u["_id"] for u in mr.u])).one_to_many("u._id", "g.user_id")
    print(list(mr))


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
