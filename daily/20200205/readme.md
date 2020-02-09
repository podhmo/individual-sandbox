## python minitask

- workerの引数がuid固定なのが辛いかもしれない
- threadingでNoneは勝手に埋めてほしい

## prestring typing

大変

## python handofcats

- ヘルプの表示がうるさい -> subcommand作る？
- parserの作成だけをしたい -> setup parserが惜しい
- parse_known_argsを使いたい時がある
- printしない関数にも適用したい

まぁいつもどおりな感じ。

## python 無限にnestできるargparse

- 基本的には再帰。
- `-h`の扱いが面倒。
- handofcatsは `parse_args()` と `cont` が固定なのでそこがちょっと残念

## python typerをいじってみたいかも

まぁふつうに使い勝手は変わらない？

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

