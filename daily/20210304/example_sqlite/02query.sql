SELECT
    json_extract(json_test_table.json, '$.attr_b') as attr_b
FROM
    json_test_table
WHERE
    json_extract(json_test_table.json, '$.attr_b') IS NOT NULL
;
