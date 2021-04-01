variable "project-name" {
  default = "xxx"
}

variable "user" {
  default = getenv("NAME", "foo")
}

variable "ans" {
  default = "1 + 2 = ${1 + 2}"
}
