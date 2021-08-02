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
