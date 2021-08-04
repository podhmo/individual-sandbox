WITH points AS (
    SELECT 1 as id, STRUCT<x int64, y int64>(10, 10) as p UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y) UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y)
)
SELECT * FROM points as t
GROUP BY
  t.id, t.p  -- Grouping by expressions of type STRUCT is not allowed at [8:9] 