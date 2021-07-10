locals {
    m = jsondecode(<<-JSON
    {"name": "foo", "age": 20}
JSON
)
}

output r {
    value  = local.m
}