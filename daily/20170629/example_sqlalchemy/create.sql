CREATE TABLE childs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);

CREATE TABLE kinds (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
);


CREATE TABLE dogs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    kind_id INTEGER,
    dog TEXT,
    FOREIGN KEY(kind_id) REFERENCES kinds(id)
);

CREATE TABLE child_dogs (
    child_id INTEGER,
    dog_id INTEGER,
    FOREIGN KEY(child_id) REFERENCES childs(id),
    FOREIGN KEY(dog_id) REFERENCES dogs(id)
);
