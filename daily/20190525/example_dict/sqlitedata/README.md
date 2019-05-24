setup

```console
rm -rf *.db
cat schema.sql | sqlite3 -echo -column -header data.db
CREATE TABLE groups (
   gid INTEGER PRIMARY KEY AUTOINCREMENT,
   gname TEXT NOT NULL
);

CREATE TABLE users (
   uid INTEGER PRIMARY KEY AUTOINCREMENT,
   gid INTEGER NOT NULL,
   uname TEXT NOT NULL,
   FOREIGN KEY(gid) REFERENCES groups(uid)
);
cat data.sql | sqlite3 -echo -column -header data.db
INSERT INTO groups VALUES(1, "A");
INSERT INTO groups VALUES(2, "B");
INSERT INTO groups VALUES(3, "C");

INSERT INTO users VALUES(10, 1, "Ax");
INSERT INTO users VALUES(11, 1, "Ay");
INSERT INTO users VALUES(20, 2, "Bi");
INSERT INTO users VALUES(40, 4, "D?");
```

query

```
****************************************
select
****************************************
select * from users
uid         gid         uname     
----------  ----------  ----------
10          1           Ax        
11          1           Ay        
20          2           Bi        
40          4           D?        
select * from groups
gid         gname     
----------  ----------
1           A         
2           B         
3           C         


****************************************
inner-join
****************************************
select * from users as u join groups as g on u.gid = g.gid
uid         gid         uname       gid         gname     
----------  ----------  ----------  ----------  ----------
10          1           Ax          1           A         
11          1           Ay          1           A         
20          2           Bi          2           B         
select * from groups as g join users as u on u.gid = g.gid
gid         gname       uid         gid         uname     
----------  ----------  ----------  ----------  ----------
1           A           10          1           Ax        
1           A           11          1           Ay        
2           B           20          2           Bi        


****************************************
left-outer-join
****************************************
select * from users as u left outer join groups as g on u.gid = g.gid
uid         gid         uname       gid         gname     
----------  ----------  ----------  ----------  ----------
10          1           Ax          1           A         
11          1           Ay          1           A         
20          2           Bi          2           B         
40          4           D?                                
select * from groups as g left outer join users as u on u.gid = g.gid
gid         gname       uid         gid         uname     
----------  ----------  ----------  ----------  ----------
1           A           10          1           Ax        
1           A           11          1           Ay        
2           B           20          2           Bi        
3           C                                             


****************************************
full-outer-join
****************************************
select * from users as u left outer join groups as g on u.gid = g.gid union select * from groups as g left outer join users as u on g.gid = u.gid where u.gid is null
uid         gid         uname       gid         gname     
----------  ----------  ----------  ----------  ----------
3           C                                             
10          1           Ax          1           A         
11          1           Ay          1           A         
20          2           Bi          2           B         
40          4           D?                                
```
