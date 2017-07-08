CREATE TABLE language (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
);
CREATE TABLE user (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    language_id INTEGER,
    FOREIGN KEY(language_id) REFERENCES language(id),
);
CREATE TABLE item (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    language_id INTEGER,
    FOREIGN KEY(language_id) REFERENCES language(id),
);
