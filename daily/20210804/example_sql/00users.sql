CREATE TEMP FUNCTION Now0()
  RETURNS TIMESTAMP
  AS (TIMESTAMP_SECONDS(1628038568));

WITH users AS (
    SELECT 1 as id, "foo" as name, Now0() as _updatedAt UNION ALL
    SELECT 2, "bar", Now0()
)
SELECT
  *
FROM
  users