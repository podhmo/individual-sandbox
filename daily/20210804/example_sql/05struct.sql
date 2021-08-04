WITH points AS (
    SELECT 1 as id, STRUCT<x int64, y int64>(10, 10) as p UNION ALL
    SELECT 2, STRUCT(0 as x, 10 as y)
)
SELECT * FROM points