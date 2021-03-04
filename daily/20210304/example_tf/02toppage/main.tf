variable "aws_region" {
  default = "us-west-1a"
}

variable "base_cidr_block" {
  description = "A /16 CIDR range definition, such as 10.1.0.0/16, that the VPC will use"
  default     = "10.1.0.0/16"
}

variable "availability_zones" {
  description = "A list of availability zones in which to create subnets"
  type        = list(string)
  default = ["us-west-1a"]
}

resource "null_resource" "main" {
  triggers = {
    # Referencing the base_cidr_block variable allows the network address
    # to be changed without modifying the configuration.
    cidr_block = var.base_cidr_block
  }
}

locals {
  c = 0
}
resource "null_resource" "az" {
  triggers = {
    # Create one subnet for each given availability zone.
    count = length(var.availability_zones)

    # For each subnet, use one of the specified availability zones.
    availability_zone = var.availability_zones[local.c]

    # By referencing the aws_vpc.main object, Terraform knows that the subnet
    # must be created only after the VPC is created.
    vpc_id = null_resource.main.id

    # Built-in functions and operators can be used for simple transformations of
    # values, such as computing a subnet address. Here we create a /20 prefix for
    # each subnet, using consecutive addresses for each availability zone,
    # such as 10.1.16.0/20 .
    cidr_block = cidrsubnet(null_resource.main.triggers.cidr_block, 4, local.c)
  }
}
