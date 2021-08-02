WITH RECURSIVE f(n, name) AS (
  SELECT 1 as n, 'f' as name
  UNION ALL SELECT n+1 as n, 'f'|| substr('oooooooooooo', 1, n) as name FROM f LIMIT 5
) SELECT * from f
