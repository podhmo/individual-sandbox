## zmq

- https://www.cuspy.org/diary/2015-05-07-zmq/
- https://qiita.com/gwappa/items/9677e1ea4adcf2d457f4

## cloud pub/sub

- https://cloud.google.com/pubsub/docs/how-to?hl=ja
- sqsと対応するものではないけれど。cloud tasksってあんまり聞かないな。そういえば。

  - https://cloud.google.com/tasks/docs/pricing?hl=ja
  - https://cloud.google.com/pubsub/pricing?hl=ja

   - どうもcloud tasksはhttp handler的なものでやるものっぽい（昔はapp engine専用だった）
   -  https://cloud.google.com/tasks/docs/creating-http-target-tasks?hl=ja

cloud tasksは無視して良いや

- https://cloud.google.com/pubsub/docs/quickstart-client-libraries?hl=ja
- https://nansystem.com/using-cloud-pub-sub-emulator/

### cloud pubsub

- https://cloud.google.com/pubsub/docs/how-to?hl=ja
## python sqs

とりあえずsqsを雑に触る

- いろいろ忘れてた
- そういえばboto3経由で使うのだっけ？
- boto3はbotocoreでurllib3だった

### worker

テキトーにworker的なものの良さそうなものを探してみる


- https://kawasin73.hatenablog.com/entry/2019/12/15/015942
- https://github.com/Nextdoor/ndkale

### elasticmq

実際のs3ばかりで試せても嬉しくないのでelasticmqとか立ち上げるか。。


- https://hub.docker.com/r/softwaremill/elasticmq/

### aio

一応s3が主というやつだけれど、aio-libsがなんか出しているっぽいな

- https://github.com/aio-libs/aiobotocore

### 本当に常用したい場合のqueue worker

- signal handling (graceful stop)
- dead letter queueへの保存 (blackhole)
- exponential backoff
- retry with specific time (retryAt)
- concurrent queue
- hooks for monitoring
- send messages as batch
