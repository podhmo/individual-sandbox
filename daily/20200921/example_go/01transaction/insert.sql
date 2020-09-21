create table user (
  id integer primary key autoincrement,
  name text not null
);
create table calc (
  id integer primary key autoincrement, 
  user_id  integer not null,
  price integer not null,
  constraint fk_user_id foreign key (user_id) references user (id)
);

begin transaction;
insert into user (id, name) values (1, 'foo');
insert into user (id, name) values (2, 'bar');

insert into calc (user_id, price) values (1, 100);
insert into calc (user_id, price) values (1, 1000);
insert into calc (user_id, price) values (1, 10000);
insert into calc (user_id, price) values (2, 200);
insert into calc (user_id, price) values (2, 2000);
commit;
