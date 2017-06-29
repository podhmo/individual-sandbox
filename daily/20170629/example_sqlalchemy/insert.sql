INSERT INTO kinds VALUES(NULL, 'x');
INSERT INTO kinds VALUES(NULL, 'y');
INSERT INTO kinds VALUES(NULL, 'z');
INSERT INTO childs VALUES(NULL, 'bobby');
SELECT last_insert_rowid(); -- gives the id of bobby, assume 2 for this example
INSERT INTO dogs VALUES(NULL, 1, 'spot');
SELECT last_insert_rowid(); -- gives the id of spot, assume 4 for this example
INSERT INTO child_dogs VALUES(2, 4);
