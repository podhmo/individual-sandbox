variable "aws_region" {
  default = "us-west-1a"
}

locals {
  twice = join(", ", [var.aws_region, var.aws_region])
  weeks = {
    su = "日"
    mo = "月"
    tu = "火"
    we = "水"
    th = "木"
    fr = "金"
    sa = "土"
  }
}

resource "null_resource" "output" {
  triggers = {
    twice = format("** %s **", local.twice)
    weeks = local.weeks
  }
}

