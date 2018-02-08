CREATE TABLE IF NOT EXISTS xs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT UNIQUE  NOT NULL
);

CREATE TABLE IF NOT EXISTS ys (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  x_id INTEGER NOT NULL,
  name TEXT UNIQUE  NOT NULL,
  FOREIGN KEY (x_id) REFERENCES xs (id)
);

CREATE TABLE IF NOT EXISTS ts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  x_id INTEGER NOT NULL,
  y_id INTEGER NOT NULL,
  memo text NOT NULL,
  FOREIGN KEY (x_id) REFERENCES xs (id),
  FOREIGN KEY (y_id) REFERENCES ys (id)
);

-- .schema
-- .headers on
INSERT INTO xs (name) VALUES ('x'), ('y'), ('z');
INSERT INTO ys (name,x_id) VALUES ('xa', 1), ('xb', 1),('ya', 1), ('yb', 1),('za', 1), ('zb', 1);
INSERT INTO ts (x_id, y_id, memo) VALUES (1, 1, 'x-a'), (1, 2, 'x-b'), (2, 3, 'y-a'), (2, 4, 'y-b'), (3, 5, 'z-a'), (3, 6, 'z-b');

-- select T.x_id, T.y_id, T.memo from ts as T;
select T.x_id, T.y_id, X.name, Y.name, T.memo from ts as T JOIN xs as X on T.x_id = X.id JOIN ys as Y on T.y_id = Y.id;

-- x_id|y_id|name|name|memo
-- 1|1|x|xa|x-a
-- 1|2|x|xb|x-b
-- 2|3|y|ya|y-a
-- 2|4|y|yb|y-b
-- 3|5|z|za|z-a
-- 3|6|z|zb|z-b

