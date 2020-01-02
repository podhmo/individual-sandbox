import os

AWS_REGION = "us-west-2"

#
# Production settings
# (use this for prod to talk to Amazon SQS)

# MESSAGE_QUEUE_USE_PROXY = False
# AWS_ACCESS_KEY_ID = 'AWS KEY ID'
# AWS_SECRET_ACCESS_KEY = ''AWS SECRET KEY

#
# Development settings
# (use this for dev to talk to ElasticMQ, which is SQS emulator)

# Using elasticmq to emulate SQS locally
MESSAGE_QUEUE_USE_PROXY = True
MESSAGE_QUEUE_PROXY_PORT = 9324
MESSAGE_QUEUE_PROXY_HOST = os.getenv("MESSAGE_QUEUE_PROXY_HOST", "0.0.0.0")
AWS_ACCESS_KEY_ID = "x"
AWS_SECRET_ACCESS_KEY = "x"

QUEUE_CONFIG = "queue_config.yaml"

# SQS limits per message size, bytes
# It can be set anywhere from 1024 bytes (1KB), up to 262144 bytes (256KB).
# See http://aws.amazon.com/sqs/faqs/
SQS_TASK_SIZE_LIMIT = 256000

QUEUE_SELECTOR = "kale.queue_selector.ReducedLottery"
