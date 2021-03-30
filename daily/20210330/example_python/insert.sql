PRAGMA foreign_keys=ON;
BEGIN TRANSACTION;
CREATE TABLE artist(
  artistid    INTEGER PRIMARY KEY NOT NULL,
  artistname  TEXT NOT NULL
);
CREATE TABLE track(
  trackid     INTEGER PRIMARY KEY NOT NULL,
  trackname   TEXT,
  trackartist INTEGER,
  FOREIGN KEY(trackartist) REFERENCES artist(artistid)
);
COMMIT;
