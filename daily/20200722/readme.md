## bot

このbotを作る [../20200721/readme.md](../20200721/readme.md)


- 状態遷移の表示 (progess)
- 標準入出力をハンドリング (timeout?)

### message

もう少し細かく見ていく

- 一度投稿したメッセージの編集
- 自分にしか見えないメッセージ

この辺を出し分ける？

- https://api.slack.com/methods/chat.postMessage
- https://api.slack.com/methods/chat.meMessage
- https://api.slack.com/methods/chat.update

どういう表示にしたいかはこれを使うと便利な模様。

https://api.slack.com/docs/messages/builder?msg=%7B%22text%22%3A%22I%20am%20a%20test%20messag%22%2C%22attachments%22%3A%5B%7B%22text%22%3A%22And%20here%E2%80%99s%20an%20at%22%7D%5D%7D

よく分かっていないオプション

- parse
- mrkdown
- reply_broadcast (これはthreadのときに使う)

blocks?/attachments?

> The usage of the text field changes depending on whether you're using blocks. If you are using blocks, this is used as a fallback string to display in notifications. If you aren't, this is the main body text of the message. It can be formatted as plain text, or with mrkdwn.

> The text field is not enforced as required when using blocks or attachments. However, we highly recommended that you include text to provide a fallback when using blocks, as described above.

- https://qiita.com/yoshitake_1201/items/9ae3a48373bfafa89e97
- https://app.slack.com/block-kit-builder

表示はある程度頑張ればどうにかなりそう。

### 状態の更新

- post_message
- update_message

するだけ

### reactionへの反応

此処を見ればわかりそう。

- https://github.com/slackapi/python-slackclient/tree/7a045607fde73d252e9e74a1b2f397b88a7d50af/tutorial


### channelが見つからない場合

```
slack.errors.SlackApiError: The request to the Slack API failed.
The server responded with: {'ok': False, 'error': 'channel_not_found'}
```

### 自分だけへのメッセージ

postEphemeral

### user

user_idとuser_nameのmappingどうすると良いんだろう？

### まじめにinteractiveな機能を有効にするためには

なんかやっぱりdeployが必要そう？

- https://api.slack.com/apps

verification tokenとかいる

- https://github.com/slackapi/python-message-menu-example
- https://qiita.com/tomomi_slack/items/21fedcc6ce07aa44a670

## gcp cloud run

- https://cloud.google.com/run/docs/quickstarts/build-and-deploy

gcrに登録しないと。

[./example_cloud_run](./example_cloud_run)
