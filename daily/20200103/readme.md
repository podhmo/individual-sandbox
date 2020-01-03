# python sqs

とりあえずsqsを雑に触る

- いろいろ忘れてた
- そういえばboto3経由で使うのだっけ？
- boto3はbotocoreでurllib3だった

## worker

テキトーにworker的なものの良さそうなものを探してみる


- https://kawasin73.hatenablog.com/entry/2019/12/15/015942
- https://github.com/Nextdoor/ndkale

## elasticmq

実際のs3ばかりで試せても嬉しくないのでelasticmqとか立ち上げるか。。


- https://hub.docker.com/r/softwaremill/elasticmq/

## aio

一応s3が主というやつだけれど、aio-libsがなんか出しているっぽいな

- https://github.com/aio-libs/aiobotocore

## 本当に常用したい場合のqueue worker

- signal handling (graceful stop)
- dead letter queueへの保存 (blackhole)
- exponential backoff
- retry with specific time (retryAt)
- concurrent queue
- hooks for monitoring
- send messages as batch
