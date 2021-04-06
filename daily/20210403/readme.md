## go openapi

refsがないバージョンならこれで済むのか。

- https://github.com/getkin/kin-openapi/blob/master/openapi3gen/simple_test.go

あと、作り途中なのかもだけどopenapi3filterを使うとvalidation効かせられるっぽい

- https://github.com/getkin/kin-openapi/blob/master/openapi3filter/validation_handler.go

JSONだけならschemaが対応している

- https://github.com/getkin/kin-openapi/blob/45c1543c7976be04a87b2f8ff4baf7116390daca/openapi3/schema.go#L752
