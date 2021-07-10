// TODO: computed property (sensitive)
terraform {
    experiments = [module_variable_optional_attrs]
}

variable "config" {
    type = object({
        person = object({
            name = string
            age = number
            nickname = optional(string)
        })
    })
}