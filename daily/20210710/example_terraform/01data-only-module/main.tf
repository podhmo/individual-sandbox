module "foo" {
    source = "./data"
    config = {
        person = jsondecode(<<-JSON
        {"name": "foo", "age": 20}
JSON        
        )
    }
}

output "foo_result" {
    value = module.foo.result
}