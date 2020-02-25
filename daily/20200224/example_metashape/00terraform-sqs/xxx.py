import typing as t
import dataclasses


@dataclasses.dataclass(frozen=True)
class aws_sqs_queue:
    name: str
    fifo_queue: t.Optional[bool] = None
    content_based_deduplication: t.Optional[bool] = None
    kms_master_key_id: t.Optional[str] = None
    kms_data_key_reuse_period_seconds: t.Optional[str] = None
    redrive_policy: t.Optional[str] = None


# fifo queue
terraform_queue = aws_sqs_queue(
    name="terraform-example-queue.fifo",
    fifo_queue=True,
    content_based_deduplication=True,
)

# sse queue
terraform_queue2 = aws_sqs_queue(
    name="terraform-example-queue",
    kms_master_key_id="alias/aws/ss",
    kms_data_key_reuse_period_seconds=300,
)

# sse queue
terraform_queue3 = aws_sqs_queue(
    name="terraform-example-queue",
    kms_master_key_id="alias/aws/ss",
    kms_data_key_reuse_period_seconds=300,
    redrive_policy={"deadLetterTargetArn": "@arn@", "maxReceiveCount": 4},
)

from to_terraform import scan, emit, emit2

resources = scan(globals())
print(emit([terraform_queue, terraform_queue2, terraform_queue3], resources=resources))

# TODO:
# resource "aws_sqs_queue" "terraform_queue" {
#   name                      = "terraform-example-queue"
#   delay_seconds             = 90
#   max_message_size          = 2048
#   message_retention_seconds = 86400
#   receive_wait_time_seconds = 10
#   redrive_policy            = jsonencode({
#     deadLetterTargetArn = aws_sqs_queue.terraform_queue_deadletter.arn
#     maxReceiveCount     = 4
#   })

#   tags = {
#     Environment = "production"
#   }
# }
print("")
print(emit2([terraform_queue, terraform_queue2, terraform_queue3], resources=resources))
