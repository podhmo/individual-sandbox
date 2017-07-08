CREATE TABLE person (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    father_id INTEGER,
    mother_id INTEGER,
    FOREIGN KEY(father_id) REFERENCES person(id),
    FOREIGN KEY(mother_id) REFERENCES person(id)
);
