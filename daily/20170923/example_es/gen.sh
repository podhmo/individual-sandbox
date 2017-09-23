seq $1 | xargs -I{} echo '{"_id": {}, "name": "foo", "value": {}}'
