SELECT
    tree.json
FROM
    json_test_table,
    json_tree(json_test_table.json) as tree
WHERE
    name = 'test_name_1'
;
