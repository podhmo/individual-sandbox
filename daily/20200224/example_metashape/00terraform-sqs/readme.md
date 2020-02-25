# terafform (sqs)

usage

```
resource "aws_sqs_queue" "terraform_queue" {
  name                      = "terraform-example-queue"
  delay_seconds             = 90
  max_message_size          = 2048
  message_retention_seconds = 86400
  receive_wait_time_seconds = 10
  redrive_policy            = jsonencode({
    deadLetterTargetArn = aws_sqs_queue.terraform_queue_deadletter.arn
    maxReceiveCount     = 4
  })

  tags = {
    Environment = "production"
  }
}
```

- https://www.terraform.io/docs/providers/aws/r/sqs_queue.html


## 何を気にしないといけないんだろう？

pythonからこれに変換できるか？ということが気になっている

存在する属性の一覧はどこに書いてあるんだろう？

- https://www.terraform.io/docs/providers/aws/r/sqs_queue.html#argument-reference

argumentsとattributesの違いは？

- https://www.terraform.io/docs/glossary.html#argument
- https://www.terraform.io/docs/glossary.html#attribute

argumentsは `<IDENTIFIER> = <EXPRESSION >` のことで、
attributesは `<IDENTIFIER>.<IDENTIFIER>` みたいなもののを指しているっぽい？

