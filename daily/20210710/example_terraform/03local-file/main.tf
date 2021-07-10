# https://registry.terraform.io/providers/hashicorp/local/latest/docs/resources/file

resource "local_file" "foo" {
    sensitive_content     = "foo!"
    filename = "${path.module}/data/foo.bar"
    file_permission = 0744
}

locals {
    person = {
        name = "foo"
        age = 20
    }
}

resource "local_file" person {
    content = jsonencode(local.person)
    filename = "${path.module}/data/person.json"
    file_permission = 0744
}