CREATE TABLE greetings (
  greeting_id INTEGER PRIMARY KEY AUTOINCREMENT,
  greeting TEXT
);

CREATE TABLE users (
  user_id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT,
  firstname TEXT,
  lastname TEXT
);

INSERT INTO greetings (greeting) VALUES 
  ('Hi'),
  ('Alpha'),
  ('Hola')
;

INSERT INTO users (username, firstname, lastname) VALUES
   ('xxx yyy', 'xxx', 'yyy'),
   ('zzz yyy', 'zzz', 'yyy')
 ;
