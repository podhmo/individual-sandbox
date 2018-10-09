import setup

with setup.run(":memory:") as c:
    # select T.x_id, T.y_id, X.name, Y.name, T.memo from ts as T JOIN xs as X on T.x_id = X.id JOIN ys as Y on T.y_id = Y.id
    for t in c.execute("select id, x_id, y_id, memo from ts").fetchall():
        for x in c.execute("select name from xs where id = ?", (t["x_id"], )).fetchall():
            for y in c.execute("select name from ys where id = ?", (t["y_id"], )).fetchall():
                print("@", t["x_id"], t["y_id"], x["name"], y["name"], t["memo"])
