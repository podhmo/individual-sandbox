resource "aws_s3_bucket" "b" {
  bucket        = "podhmo-my-tf-test-bucket"
  acl           = "private"
  force_destroy = true

  tags = {
    Name        = "My bucket"
    Environment = "Dev"
  }
}
