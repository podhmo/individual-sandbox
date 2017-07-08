CREATE TABLE shared (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
);
CREATE TABLE xxx (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    good_shared_id INTEGER,
    bad_shared_id INTEGER,
    FOREIGN KEY(good_shared_id) REFERENCES shared(id),
    FOREIGN KEY(bad_shared_id) REFERENCES shared(id)
);
CREATE TABLE yyy (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    good_shared_id INTEGER,
    bad_shared_id INTEGER,
    FOREIGN KEY(good_shared_id) REFERENCES shared(id),
    FOREIGN KEY(bad_shared_id) REFERENCES shared(id)
);
