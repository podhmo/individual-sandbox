## orator

```
$ pip install orator
```

- nullableをなくす方法はどうする？
- foreign keyの設定は？
- database driverをどうやってイジる？
- 実行されるSQLのtraceどこにあるんだろう？
- raw sqlをどうやって実行すれば良いんだろう？

## migrations

本来は

```
$ orator make:migration create_users_table --table=users --create
```

## oratorの使いかたの整理

packageが提供しているコマンドってどうやって探せば良いんだろう？

```
# oratorコマンドが使える
$ orator make
```
