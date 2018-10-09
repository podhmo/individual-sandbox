import setup

with setup.run(":memory:") as c:
    for x in c.execute("select id, name from xs").fetchall():
        for t in c.execute("select y_id, memo from ts where x_id = ?", (x["id"], )).fetchall():
            for y in c.execute("select id, name from ys where id = ?", (t["y_id"], )).fetchall():
                print("@", x["id"], y["id"], x["name"], y["name"], t["memo"])
