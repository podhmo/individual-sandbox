SELECT
    name
FROM
    json_test_table
ORDER BY
    json_extract(json, '$.key_for_str') DESC
;
