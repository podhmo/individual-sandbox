import setup

with setup.run(":memory") as c:
    for row in c.execute(
        """
        select T.x_id, T.y_id, X.name, Y.name, T.memo from ts as T JOIN xs as X on T.x_id = X.id JOIN ys as Y on T.y_id = Y.id
            """
    ):
        print(dict(row))
