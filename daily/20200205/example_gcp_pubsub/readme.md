# pub/subを使いたい

## google-cloud-sdk

初手はこういうエラーが出る。

```console
$HOME/vboxshare/venvs/gcloud/google-cloud-sdk/lib/googlecloudsdk/core/console/console_io.py:543: SyntaxWarning: "is" with a literal. Did you mean "=="?
  if answer is None or (answer is '' and default is not None):
Traceback (most recent call last):
  File "$HOME/vboxshare/venvs/gcloud/./google-cloud-sdk/bin/bootstrapping/install.py", line 27, in <module>
    from googlecloudsdk import gcloud_main
  File "$HOME/vboxshare/venvs/gcloud/google-cloud-sdk/lib/googlecloudsdk/gcloud_main.py", line 37, in <module>
    from googlecloudsdk.command_lib.util.apis import yaml_command_translator
  File "$HOME/vboxshare/venvs/gcloud/google-cloud-sdk/lib/googlecloudsdk/command_lib/util/apis/yaml_command_translator.py", line 236
    if self.spec.async:
                 ^
SyntaxError: invalid syntax
make: *** [install] Error 1
```

こういう変更が必要(利用しているのは [`_install.mk`](./_install.mk))。
ついでにsymlinkも貼っている。

### どこに何があるか

- gcloudの設定は `~/.config/gcloud` ?
- gcloudのコマンド自体は `./google-cloud-sdk/bin` 以下に

環境変数を設定しておけるもよう

```console
$ export CLOUDSDK_PYTHON=$(shell which python)
```


### components

初回で紐付けるっぽい。ここで利用するprojectのデフォルトが設定できる。

```
$ gcloud components update
$ gcloud init
```

（環境変数で一時的に変えられる模様 (e.g. `GOOGLE_APPLICATION_CREDENTIALS`）)

initの振る舞いはこの辺に

- https://cloud.google.com/sdk/docs/initializing

## こういう動作確認ができるのは良い

良いですね。

```console
$ gcloud pubsub topics list
$ gcloud pubsub topics create my-topic
Created topic [projects/steady-voltage-181003/topics/my-topic].
$ gcloud pubsub subscriptions create --topic my-topic my-sub
Created subscription [projects/steady-voltage-181003/subscriptions/my-sub].
$ gcloud pubsub topics publish my-topic --message "hello"
messageIds:
- '965957658141069'
$ gcloud pubsub subscriptions pull --auto-ack my-sub
┌───────┬─────────────────┬────────────┐
│  DATA │    MESSAGE_ID   │ ATTRIBUTES │
├───────┼─────────────────┼────────────┤
│ hello │ 965957658141069 │            │
```

:memo: topicのあとsubscriptionsを作らないとだめ。それはそう。

## コードを書く

### 認証

そのまま利用すると認証がないのでエラーになる。

```
google.auth.exceptions.DefaultCredentialsError: Could not automatically determine credentials. Please set GOOGLE_APPLICATION_CREDENTIALS or explicitly create credentials and re-run the application. For more information, please see https://cloud.google.com/docs/authentication/getting-started
```

手元にファイルがない状態のときに良い感じにgcloudコマンドを利用して解決できないか？

- https://cloud.google.com/sdk/docs/authorizing?hl=ja
- https://cloud.google.com/docs/authentication/getting-started#setting_the_environment_variable
- https://google-auth.readthedocs.io/en/latest/user-guide.html

...

めんどくさくなったので素直にコンソールからservice accountを作った。
exportで GOOGLE_APPLICATION_CREDENTIALS を指定した。

```
google.api_core.exceptions.PermissionDenied: 403 Cloud Pub/Sub API has not been used in project 425471851367 before or it is disabled. Enable it by visiting https://console.developers.google.com/apis/api/pubsub.googleapis.com/overview?project=425471851367 then retry. If you enabled this API recently, wait a few minutes for the action to propagate to our systems and retry.
```

その後API自体の権限を要求する部分があった。

### やっていく

- https://github.com/googleapis/python-pubsub
- https://googleapis.dev/python/pubsub/latest/index.html

### topicの作成

topicの作成にはproject-idとtopic-nameが必要。

project idを取るにはgcloudコマンドを使うと便利。

```console
$ gcloud projects list
PROJECT_ID             NAME        PROJECT_NUMBER
disco-freedom-240305   My Project  597800272932
steady-voltage-181003  My Project  425471851367
```


### batch 処理

https://googleapis.dev/python/pubsub/latest/publisher/index.html#batching

## 削除は

それぞれtopicとsubscriptionsを別々にやる。

