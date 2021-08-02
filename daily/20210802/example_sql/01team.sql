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
