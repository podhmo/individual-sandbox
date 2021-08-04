CREATE TEMP FUNCTION Now0()
  RETURNS TIMESTAMP
  AS (TIMESTAMP_SECONDS(1628038568));

CREATE TEMP FUNCTION Now1()
  RETURNS TIMESTAMP
  AS (TIMESTAMP_SUB(Now0(), INTERVAL 1 DAY));


WITH users AS (
    SELECT 1 as id, "foo" as name, Now0() as _updatedAt UNION ALL
    SELECT 2, "bar", Now0() UNION ALL
    -- これが過去のもの
    SELECT 1 as id, "fo" as name, Now1()
)
SELECT
  *
FROM
  users  