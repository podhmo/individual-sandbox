variable "projec-tname" {
  default = "xxx"
}

variable "user" {
  default = hello("foo")
}

variable "ans" {
  default = "1 + 2 = ${1 + 2}"
}
