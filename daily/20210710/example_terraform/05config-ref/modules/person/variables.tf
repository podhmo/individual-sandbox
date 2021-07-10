variable "savefile" {
    type = string
    default = ""
    description = "path of data if this value is empty, act as loading"
}

variable "config" {
    type = object({
        name = string
        age = number
    })
}
