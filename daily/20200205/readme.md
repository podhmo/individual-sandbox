## python gcloud pubsub

- https://cloud.google.com/pubsub/docs/quickstart-console?hl=ja
- https://cloud.google.com/pubsub/docs/quickstart-cli?hl=ja

### gcloudのインストール

- https://cloud.google.com/sdk/docs/?hl=ja

なんかどっかから取ってきてインストールとかしかない？だるい。

### gcloudで利用

gcloudのインストールが必要

```console
gcloud init
gcloud pubsub topics create my-topic
gcloud pubsub subscriptions create --topic my-topic my-sub
gcloud pubsub topics publish my-topic --message "hello"
gcloud pubsub subscriptions pull --auto-ack my-sub
```

## python example sqs

- subprocessだけに頼ったものを作る
- (handofcatsにだけ依存しておく）


## metashape hmm

We have python, have typing, and need more input?

とかなのかな？

