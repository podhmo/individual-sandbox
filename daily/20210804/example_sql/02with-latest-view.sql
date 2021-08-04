CREATE TEMP FUNCTION Now0()
  RETURNS TIMESTAMP
  AS (TIMESTAMP_SECONDS(1628038568));

CREATE TEMP FUNCTION Now1()
  RETURNS TIMESTAMP
  AS (TIMESTAMP_SUB(Now0(), INTERVAL 1 DAY));


WITH users AS (
  SELECT 1 as id, "foo" as name, Now0() as _updatedAt UNION ALL
  SELECT 2, "bar", Now0() UNION ALL
  SELECT 1 as id, "fo" as name, Now1()
),
users_latest AS ( -- 実際はviewなど
  SELECT
   * 
  FROM users as x
  INNER JOIN (SELECT DISTINCT o.id, MAX(o._updatedAt) as _updatedAt FROM users as o GROUP BY id) as y ON x.id = y.id AND x._updatedAt = y._updatedAt
)
SELECT
  *
FROM
  users_latest