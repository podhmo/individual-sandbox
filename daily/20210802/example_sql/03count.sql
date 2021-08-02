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
  count(*)
FROM
  teams as t
  INNER JOIN users as u ON t.id = u.team_id
GROUP BY
  t.id
