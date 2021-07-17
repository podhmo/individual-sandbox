data "terraform_remote_state" "root" {
    backend = "local"
    config = {
        path = "../terraform.tfstate"
    }
}

module "person" {
    source = "../modules/person"
    config = jsondecode(file("../data/foo.json"))
}


output "message" {
    value = "hello ${module.person.config.name}"    
    # value = "hello ${data.terraform_remote_state.root.outputs.foo_config.name}"
}