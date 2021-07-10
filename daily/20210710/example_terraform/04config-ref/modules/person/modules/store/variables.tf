variable "filename" {
    type = string
}

variable "config" {
    type = object({
        name = string
        age = number
    })
}