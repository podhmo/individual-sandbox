INSERT INTO child VALUES(NULL, 'bobby');
SELECT last_insert_rowid(); -- gives the id of bobby, assume 2 for this example
INSERT INTO dog VALUES(NULL, 'spot');
SELECT last_insert_rowid(); -- gives the id of spot, assume 4 for this example
INSERT INTO child_dog VALUES(2, 4);
