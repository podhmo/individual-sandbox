import sqlite3
import contextlib

QUERIES = [
    """
    CREATE TABLE IF NOT EXISTS xs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE  NOT NULL
    )
    """, """
    CREATE TABLE IF NOT EXISTS ys (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      x_id INTEGER NOT NULL,
      name TEXT UNIQUE  NOT NULL,
      FOREIGN KEY (x_id) REFERENCES xs (id)
    )
    """, """
    CREATE TABLE IF NOT EXISTS ts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      x_id INTEGER NOT NULL,
      y_id INTEGER NOT NULL,
      memo text NOT NULL,
      FOREIGN KEY (x_id) REFERENCES xs (id),
      FOREIGN KEY (y_id) REFERENCES ys (id)
    )
    """, """
    INSERT INTO xs (name) VALUES ('x'), ('y'), ('z');
    """, """
    INSERT INTO ys (name,x_id) VALUES ('xa', 1), ('xb', 1),('ya', 1), ('yb', 1),('za', 1), ('zb', 1)
    """, """
    INSERT INTO ts (x_id, y_id, memo) VALUES (1, 1, 'x-a'), (1, 2, 'x-b'), (2, 3, 'y-a'), (2, 4, 'y-b'), (3, 5, 'z-a'), (3, 6, 'z-b');
    """
]


def setup(c: sqlite3.Cursor) -> None:
    for q in QUERIES:
        c.execute(q)


@contextlib.contextmanager
def run(dbpath, *, setup=setup):
    sqlite3.enable_callback_tracebacks(True)
    with sqlite3.connect(dbpath) as conn:
        conn.row_factory = sqlite3.Row
        try:
            conn.set_trace_callback(print)
            c = conn.cursor()
            setup(c)
            yield c
        except Exception:
            conn.rollback()
            raise
        else:
            conn.commit()
