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
