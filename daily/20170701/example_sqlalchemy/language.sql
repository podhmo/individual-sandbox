CREATE TABLE IF NOT EXISTS "versionname" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "language_id" integer NULL REFERENCES "language" ("id"),
  "version_id" integer NULL REFERENCES "version" ("id"),
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "language" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "iso639" varchar(2) NOT NULL,
  "iso3166" varchar(2) NOT NULL,
  "official" bool NOT NULL,
  "order" integer NULL,
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "version" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "version_group_id" integer NULL REFERENCES "versiongroup" ("id"),
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "versiongroup" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "order" integer NULL,
  "generation_id" integer NULL REFERENCES "generation" ("id"),
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "generation" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "region_id" integer NULL UNIQUE REFERENCES "region" ("id"),
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "region" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "name" varchar(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS "languagename" (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "language_id" integer NULL REFERENCES "language" ("id"),
  "local_language_id" integer NULL REFERENCES "language" ("id"),
  "name" varchar(100) NOT NULL
);
