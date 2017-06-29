CREATE TABLE child (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);


CREATE TABLE dog (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    dog TEXT
);

CREATE TABLE child_dog (
    child_id INTEGER,
    dog_id INTEGER,
    FOREIGN KEY(child_id) REFERENCES child(id),
    FOREIGN KEY(dog_id) REFERENCES dog(id)
);
