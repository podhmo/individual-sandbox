-- https://gist.github.com/syzdek/956669/15ad5f0ae23591e67467f0b0aeb085b57a448f13
--
--  Simple SQLite3 Example
--  Copyright (C) 2011 David M. Syzdek <david@syzdek.net>
--
--  Run:
--     cat hello.sql | sqlite3 hello.db
--


-- creates tables
CREATE TABLE IF NOT EXISTS people (
   peopleId INTEGER PRIMARY KEY AUTOINCREMENT,
   gn TEXT,
   sn TEXT,
   birthday DATE
);

CREATE TABLE IF NOT EXISTS peopleLog (
   peopleLogId INTEGER PRIMARY KEY AUTOINCREMENT,
   peopleIdNew INTEGER,
   peopleIdOld INTEGER,
   gnNew TEXT,
   gnOld TEXT,
   snNew TEXT,
   snOld TEXT,
   birthdayNew DATE,
   birthdayOld DATE,
   action TEXT,
   timestamp DATE,
   FOREIGN KEY(peopleIdOld) REFERENCES people(peopleId),
   FOREIGN KEY(peopleIdNew) REFERENCES people(peopleId)
);

CREATE TABLE IF NOT EXISTS books (
   bookId INTEGER PRIMARY KEY AUTOINCREMENT,
   title TEXT,
   authorId INTEGER,
   FOREIGN KEY(authorId) REFERENCES people(peopleId)
);

CREATE TABLE IF NOT EXISTS booksLog (
   booksLogId INTEGER PRIMARY KEY AUTOINCREMENT,
   bookIdNew INTEGER,
   bookIdOld INTEGER,
   titleNew TEXT,
   titleOld TEXT,
   authorOld INTEGER,
   authorNew INTEGER,
   action TEXT,
   timestamp DATE,
   FOREIGN KEY(bookIdOld) REFERENCES books(bookId),
   FOREIGN KEY(bookIdNew) REFERENCES books(bookId),
   FOREIGN KEY(authorOld) REFERENCES people(peopleId),
   FOREIGN KEY(authorNew) REFERENCES people(peopleId)
);


-- Adds triggers to log all transactions
CREATE TRIGGER IF NOT EXISTS people_insert AFTER INSERT ON people
BEGIN
   INSERT INTO peopleLog (peopleIdNew, gnNew, snNew, birthdayNew, action, timestamp)
               values (new.peopleId, new.gn, new.sn, new.birthday, 'INSERT', DATETIME('NOW'));
END;
CREATE TRIGGER IF NOT EXISTS people_update AFTER UPDATE ON people
BEGIN
   INSERT INTO peopleLog (peopleIdNew, gnNew, snNew, birthdayNew,
                        peopleIdOld, gnOld, snOld, birthdayOld,
                        action, timestamp)
               values (new.peopleId, new.gn, new.sn, new.birthday,
                       old.peopleId, old.gn, old.sn, old.birthday,
                       'UPDATE', DATETIME('NOW'));
END;
CREATE TRIGGER IF NOT EXISTS people_delete AFTER DELETE ON people
BEGIN
   INSERT INTO peopleLog (peopleIdOld, gnOld, snOld, birthdayOld, action, timestamp)
               values (old.peopleId, old.gn, old.sn, old.birthday, 'DELETE', DATETIME('NOW'));
END;


CREATE TRIGGER IF NOT EXISTS book_update AFTER INSERT ON books
BEGIN
   INSERT INTO booksLog (bookIdNew, titleNew, authorNew, action, timestamp)
               values (new.bookId, new.title, new.authorId, 'INSERT', DATETIME('NOW'));
END;
CREATE TRIGGER IF NOT EXISTS book_update AFTER UPDATE ON books
BEGIN
   INSERT INTO booksLog (bookIdOld, titleOld, authorOld,
                        bookIdNew, titleNew, authorNew,
                        action, timestamp)
               values (old.bookId, old.title, old.authorId,
                       new.bookId, new.title, new.authorId,
                       'UPDATE', DATETIME('NOW'));
END;
CREATE TRIGGER IF NOT EXISTS book_update AFTER DELETE ON books
BEGIN
   INSERT INTO booksLog (bookIdOld, titleOld, authorOld, action, timestamp)
               values (old.bookId, old.title, old.authorId, 'DELETE', DATETIME('NOW'));
END;
   

-- Adds sample data into table "people"
INSERT INTO people(gn,sn)          VALUES('Russell',  "Janney");
INSERT INTO people(gn,sn,birthday) VALUES('Louis',    "L'Amour",date('1908-03-22'));
INSERT INTO people(gn,sn,birthday) VALUES('Ken',      "Follett",date('1949-06-05'));
INSERT INTO people(gn,sn,birthday) VALUES('Alexandre',"Dumas",  date('1802-07-24'));
INSERT INTO people(gn,sn,birthday) VALUES('Ambrose',  "Bierce", date('1842-06-24'));
INSERT INTO people(gn,sn,birthday) VALUES('Douglas',  "Adam",   date('1952-03-11'));
INSERT INTO people(gn,sn,birthday) VALUES('Victor',   "Hugo",   date('1802-02-26'));


-- Updates sample data in table "people"
UPDATE people SET sn = 'Adams' where gn = 'Douglas' AND sn = 'Adam';


-- Adds sample data into table "books"
INSERT INTO books(title, authorId) VALUES('Miracle of the Bells',       1);
INSERT INTO books(title, authorId) VALUES('The Walking Drum',           2);
INSERT INTO books(title, authorId) VALUES('Pillars of the Earth',       3);
INSERT INTO books(title, authorId) VALUES('The Count of Monte Cristo',  4);
INSERT INTO books(title, authorId) VALUES('The Devils Dictionary',      5);
INSERT INTO books(title, authorId) VALUES('The Hitchhikers Guide',      6);
INSERT INTO books(title, authorId) VALUES('The Hunchback of Notre Dame', 7);


-- Example queries
select books.title, people.gn, people.sn
   FROM people
   INNER JOIN books
   ON books.authorId=people.peopleId
   WHERE books.title LIKE '%Hunchback%'
   ORDER BY people.sn;


-- end of script

