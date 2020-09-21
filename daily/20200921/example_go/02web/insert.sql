create table users (
  id integer primary key autoincrement,
  name text not null
);
create table skills (
  id integer primary key autoincrement,
  user_id  integer not null,
  name text not null,
  constraint fk_user_id foreign key (user_id) references user (id)
);

begin transaction;
insert into users (id, name) values (1, 'foo');
insert into users (id, name) values (2, 'bar');

insert into skills (user_id, name) values (1, 'x');
insert into skills (user_id, name) values (1, 'y');
insert into skills (user_id, name) values (1, 'z');
insert into skills (user_id, name) values (2, 'x');
insert into skills (user_id, name) values (2, 'y');
commit;
