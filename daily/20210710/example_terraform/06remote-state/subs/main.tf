data "terraform_remote_state" "root" {
    backend = "local"
    config = {
        path = "../terraform.tfstate"
    }
}

module "person" {
    source = "../modules/person"
    config = data.terraform_remote_state.root.outputs.foo_config
}


output "message" {
    value = "hello ${module.person.config.namee}"
}