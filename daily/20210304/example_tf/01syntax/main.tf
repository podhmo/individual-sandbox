resource null_resource "example" {
  triggers = {
    hello = "hello message"
    joins = join(", ", [1,2,3,4,5])
  }
}
