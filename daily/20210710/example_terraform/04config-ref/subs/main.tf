data "terraform_remote_state" "root" {
    backend = "local"
    config = {
        path = "../terraform.tfstate"
    }
}

module "person" {
    source = "../modules/person"
    filename = data.terraform_remote_state.root.outputs.person_path
}


output "message" {
    value = "hello ${module.person.config.name}"
}