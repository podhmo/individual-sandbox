-- name: getTarget :one
SELECT percentile_disc(0.75) WITHIN GROUP(ORDER BY lcp)::real P75, 1 as one
;
