WITH points AS (
    SELECT 1 as id, STRUCT<x int64, y int64>(10, 10) as p UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y) UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y)
)
SELECT
  t.id,
--  t.p, -- SELECT list expression references t.p which is neither grouped nor aggregated at [8:3]
  t.p.x,
  t.p.y
FROM points as t
GROUP BY
  t.id, t.p.x, t.p.y

