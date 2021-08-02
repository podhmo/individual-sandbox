#[sql][memo]SQLでtableにデータをINSERTせずにqueryを確かめたい。あるいはSQLの復習。

主に手抜きのため。bigqueryのドキュメントの実行例で使われていてなるほどなーと思ったのでメモ。
ついでに色々なJOINの利用例を列挙してみる。

## WITH句を使って仮想的なtableと見做す

WITH句自体は概ねどのような処理系でもサポートしているみたい。これを使うのが今回の肝。

- bigquery https://cloud.google.com/bigquery/docs/reference/standard-sql/query-syntax?hl=ja#with_clause
- sqlite https://www.sqlite.org/lang_with.html
- postgresql https://www.postgresql.jp/document/9.0/html/queries-with.html
- mysql https://dev.mysql.com/doc/refman/8.0/en/with.html

例えば、以下はbigqueryでのコードの実行例。データとqueryが全て1つの文に入っているので状況がわかりやすい。

https://cloud.google.com/bigquery/docs/reference/standard-sql/timestamp_functions?hl=ja#extract

```sql
WITH Timestamps AS (
  SELECT TIMESTAMP("2005-01-03 12:34:56+00") AS timestamp_value UNION ALL
  SELECT TIMESTAMP("2007-12-31 12:00:00+00") UNION ALL
  SELECT TIMESTAMP("2009-01-01 12:00:00+00") UNION ALL
  SELECT TIMESTAMP("2009-12-31 12:00:00+00") UNION ALL
  SELECT TIMESTAMP("2017-01-02 12:00:00+00") UNION ALL
  SELECT TIMESTAMP("2017-05-26 12:00:00+00")
)
SELECT
  timestamp_value,
  EXTRACT(ISOYEAR FROM timestamp_value) AS isoyear,
  EXTRACT(ISOWEEK FROM timestamp_value) AS isoweek,
  EXTRACT(YEAR FROM timestamp_value) AS year,
  EXTRACT(WEEK FROM timestamp_value) AS week
FROM Timestamps
ORDER BY timestamp_value;

-- Results may differ, depending upon the environment and time zone where this query was executed.
+-------------------------+---------+---------+------+------+
| timestamp_value         | isoyear | isoweek | year | week |
+-------------------------+---------+---------+------+------+
| 2005-01-03 12:34:56 UTC | 2005    | 1       | 2005 | 1    |
| 2007-12-31 12:00:00 UTC | 2008    | 1       | 2007 | 52   |
| 2009-01-01 12:00:00 UTC | 2009    | 1       | 2009 | 0    |
| 2009-12-31 12:00:00 UTC | 2009    | 53      | 2009 | 52   |
| 2017-01-02 12:00:00 UTC | 2017    | 1       | 2017 | 1    |
| 2017-05-26 12:00:00 UTC | 2017    | 21      | 2017 | 21   |
+-------------------------+---------+---------+------+------+
```

（以降は諸々の関係上sqliteでの実行結果になっている）

## 通常のCTEの使い方

ちなみに普通はどうやって使うかと言えば、RECURSIVE付きで再帰的に問い合わせるようなもの。

```
WITH RECURSIVE f(n, name) AS (
  SELECT 1 as n, 'f' as name
  UNION ALL SELECT n+1 as n, 'f'|| substr('oooooooooooo', 1, n) as name FROM f LIMIT 10
) SELECT * from f
```

基底状態とUNION ALLで再帰したqueryを作る

```
    n = 1
 name = f

    n = 2
 name = fo

    n = 3
 name = foo

    n = 4
 name = fooo

    n = 5
 name = foooo
```

## queryの実行例

そんなわけでこれらを悪用してqueryの練習をしてみる。team,userみたいなテーブルがあるとする。テキトーにjoinを試してみることにしよう。

### joinなし

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
)
SELECT
  id,
  name
FROM
 teams
```

普通のquery。withを除けば既存のSELECT文の例に近い。

```
   id = 1
 name = x

   id = 2
 name = y

   id = 3
 name = z
```

### INNER JOIN

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
),
users as (
  SELECT 10 as id, 'foo' as name, 1 as team_id
  UNION ALL SELECT 20, 'bar', 1
  UNION ALL SELECT 30, 'boo', 3
)
SELECT
  t.id as team_id,
  t.name as team_name,
  u.id as user_id,
  u.name as user_name
FROM
  teams as t
  INNER JOIN users as u ON t.id = u.team_id
```

よくある例。

```
  team_id = 1
team_name = x
  user_id = 10
user_name = foo

  team_id = 1
team_name = x
  user_id = 2
user_name = bar

  team_id = 3
team_name = z
  user_id = 3
user_name = boo
```

teamごとのuser数なんかを数えたい場合。

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
),
users as (
  SELECT 10 as id, 'foo' as name, 1 as team_id
  UNION ALL SELECT 20, 'bar', 1
  UNION ALL SELECT 30, 'boo', 3
)
SELECT
  t.id as team_id,
  t.name as team_name,
  count(*) as c
FROM
  teams as t
  INNER JOIN users as u ON t.id = u.team_id
GROUP BY
  t.id
```

```
  team_id = 1
team_name = x
        c = 2

  team_id = 3
team_name = z
        c = 1
```

### LEFT OUTER JOIN

userを持たないteamの分も数えたいのでouter joinを使う。対応する行が存在しない側は値がNULLになるのでcountではなくsumで数える。

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
),
users as (
  SELECT 10 as id, 'foo' as name, 1 as team_id
  UNION ALL SELECT 20, 'bar', 1
  UNION ALL SELECT 30, 'boo', 3
)
SELECT
  t.id as team_id,
  t.name as team_name,
  sum(CASE WHEN u.id is NULL then 0 ELSE 1 END) as c
FROM
  teams as t
  LEFT OUTER JOIN users as u ON t.id = u.team_id
GROUP BY
  t.id
```

```
  team_id = 1
team_name = x
        c = 2

  team_id = 2
team_name = y
        c = 1

  team_id = 3
team_name = z
        c = 1
```

### many to many

多対多の関係の場合には中間テーブルが必要になる。

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
),
users as (
  SELECT 10 as id, 'foo' as name -- x,yに所属
  UNION ALL SELECT 20, 'bar' -- x,zに所属
  UNION ALL SELECT 30, 'boo' -- xに所属
),
teams2users as (
  SELECT 1 as team_id, 10 as user_id
  UNION ALL SELECT 1, 20
  UNION ALL SELECT 1, 30
  UNION ALL SELECT 2, 10
  UNION ALL SELECT 3, 20
)
SELECT
  t.id as team_id,
  t.name as team_name,
  u.id as user_id,
  u.name as user_name
FROM
  teams as t
  INNER JOIN users as u
  INNER JOIN teams2users as xref ON t.id = xref.team_id AND xref.user_id = u.id
```

```
  team_id = 1
team_name = x
  user_id = 10
user_name = foo

  team_id = 1
team_name = x
  user_id = 20
user_name = bar

  team_id = 1
team_name = x
  user_id = 30
user_name = boo

  team_id = 2
team_name = y
  user_id = 10
user_name = foo

  team_id = 3
team_name = z
  user_id = 20
user_name = bar
```

### CROSS JOIN

tableの行数とqueryの行数が乖離したようなsummary的なものを１発で作りたい時。
例えば、teamに対する所属期間のような列があるとして、それを年次で集計したい場合など

そういう実用的な例の前にトリビアルな例を。基本的には直積なので3行のものと3行のものを組み合わせたら9行のものができる。

```SQL
WITH ids AS (
  (SELECT 1 as id UNION ALL SELECT 2 UNION ALL 3)
), ys AS (
  (SELECT 'foo' as name UNION ALL SELECT 'bar' UNION ALL 'boo')
)
SELECT * FROM ids CROSS JOIN ys
```

はい。

```
1           foo       
1           bar       
1           boo       
2           foo       
2           bar       
2           boo       
3           foo       
3           bar       
3           boo       
```

これを使って以下をやってみる。

> 例えば、teamに対する所属期間のような列があるとして、それを年次で集計したい場合など

```sql
WITH teams AS (
  SELECT 1 as id, 'x' as name
  UNION ALL SELECT 2, 'y'
  UNION ALL SELECT 3, 'z'
),
users as (
  SELECT 10 as id, 'foo' as name -- x,yに所属
  UNION ALL SELECT 20, 'bar' -- x,zに所属
  UNION ALL SELECT 30, 'boo' -- xに所属
),
teams2users as (
  SELECT 1 as team_id, 10 as user_id, '2019' as start, '2020' as `end`
  UNION ALL SELECT 1, 20, '2020', '2020'
  UNION ALL SELECT 1, 30, '2020', NULL
  UNION ALL SELECT 2, 10, '2020', NULL
  UNION ALL SELECT 3, 20,  '2021', '2021'
),
years as (
  SELECT '2019' as year
  UNION ALL SELECT '2020'
  UNION ALL SELECT '2021'
  UNION ALL SELECT '2022'
)
SELECT
  y.year as year,
  t.name as team_name,
  sum(CASE WHEN u.id is NULL then 0 ELSE 1 END) as c,
  group_concat(u.name) as names
FROM
  teams as t
  INNER JOIN teams2users as xref ON t.id = xref.team_id
  LEFT OUTER JOIN users as u ON xref.user_id = u.id
  CROSS JOIN years as y ON xref.start <= y.year AND (xref.`end` IS NULL OR xref.`end` >= y.year)
GROUP BY
  y.year, t.id, t.name
```

```
     year = 2019
team_name = x
        c = 1
    names = foo

     year = 2020
team_name = x
        c = 3
    names = foo,bar,boo

     year = 2020
team_name = y
        c = 1
    names = foo

     year = 2021
team_name = x
        c = 1
    names = boo

     year = 2021
team_name = y
        c = 1
    names = foo

     year = 2021
team_name = z
        c = 1
    names = bar

     year = 2022
team_name = x
        c = 1
    names = boo

     year = 2022
team_name = y
        c = 1
    names = foo
```

### misc

DISTINCT, having, ORDER BY, window関数とかは省略。

## gist

