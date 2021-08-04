WITH points AS (
    SELECT 1 as id, STRUCT<x int64, y int64>(10, 10) as p UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y) UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y)
)
SELECT
  t.id,
  STRUCT(t.p.x as x, t.p.y as y) as p
FROM points as t
GROUP BY
  t.id, t.p.x, t.p.y