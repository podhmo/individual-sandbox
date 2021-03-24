## python cloud functions auth

これが答えな気がするがこれだけだとわからないと言う話だよなー。

- https://stackoverflow.com/questions/43456630/access-google-drive-api-from-a-google-cloud-function

分割してみる

- 普通にgoogle driveにアクセスする方法ってどうやるんだっけ？
- 普通にcloud functionsを使う方法ってどうやるんだっけ？
- 2つを混ぜる方法ってどうやるんだっけ？

### service account でログイン

service accountを使うと楽と言われている。

- https://cloud.google.com/iam/docs/service-accounts#user-managed

これでログインする方法は。。

```py
from google.oauth2.service_account import Credentials

SCOPES = ["https://www.googleapis.com/auth/drive.metadata.readonly"]


creds = Credentials.from_service_account_file(token_file)
scoped = creds.with_scopes(SCOPES)
service = build("drive", "v3", credentials=scoped)
```
- https://google-auth.readthedocs.io/en/latest/user-guide.html
- https://google-auth.readthedocs.io/en/latest/reference/google.oauth2.service_account.html

```
googleapiclient.errors.HttpError: <HttpError 403 when requesting https://www.googleapis.com/drive/v3/files?pageSize=10&fields=nextPageToken%2C+files%28id%2C+name%29&alt=json returned "Access Not Configured. Drive API has not been used in project xxxxxxxxxxxx before or it is disabled. Enable it by visiting https://console.developers.google.com/apis/api/drive.googleapis.com/overview?project=xxxxxxxxxxxx then retry. If you enabled this API recently, wait a few minutes for the action to propagate to our systems and retry.">
```

activateした。空だけど自分のドライブを参照させるのはどうするのだっけ？

- https://help.talend.com/r/E3i03eb7IpvsigwC58fxQg/uEUUsDd_MSx64yoJgSa1xg

めんどくさいからテキトーにファイルをアップロードした。見られるファイルの内容が何かservice accountだと違うらしい。

### cloud functions

- https://cloud.google.com/functions/docs/first-python
- https://cloud.google.com/functions/docs/quickstart-python


単純にはsecret managerだがそれが不要なのか？

- https://cloud.google.com/functions/docs/env-var
- https://cloud.google.com/secret-manager/docs/overview

### gcloud

```console
$ gcloud auth login
Your current project is [xxxxxxxxxxxxxxxxxxxxx].  You can change this setting by running:
  $ gcloud config set project PROJECT_ID
$ gcloud set <project id>
```

そのあとdeploy出来た `--service-account` で特別なアカウントを利用することもできるがデフォルトのもので良い？

- https://cloud.google.com/functions/docs/concepts/iam

Default compute service account

### service account付き

普通にdefaultで指定してやれば良いのでは？あー、requirements.txtを追加し忘れてた

```
$ gcloud functions logs read
..
ModuleNotFoundError: No module named 'google.auth'
```

出来た

`--service-account` 指定なし

```
{
    "error": null,
    "files": [],
    "me": "xxxxxxxxxxxxxxxxxxxxx@appspot.gserviceaccount.com",
    "project": "xxxxxxxxxxxxxxxxxxxxx"
}
```

`--service-account` 指定あり

```
{
    "error": null,
    "files": [
        {
            "id": "1p0q6zsIuGkDiQLVwPGfvK1D-lgq4-Jkl",
            "name": "gdrive-upload-example.py"
        }
    ],
    "me": "gdrive-sandbox@xxxxxxxxxxxxxxxxxxxxx.iam.gserviceaccount.com",
    "project": "xxxxxxxxxxxxxxxxxxxxx"
}
```

### 翻訳

> 私は主にPythonを使って開発していますが、Pythonには、あなたがおっしゃったNodeと同じような問題があります。また、G SuiteのAPI（Driveなど）へのアクセスはGCPのAPIとは異なるという問題もあります。(disCLAIMER: 私はGoogleで働いており、この矛盾を改善しようとしています。)

> GCFからDriveのAPIにアクセスするには、サービスアカウントのIDが最適です。しかし、Cloud FunctionのデフォルトのサービスアカウントID（App Engineアプリの場合も同じ）、またはここの別の回答で提案されているように、代わりにユーザーが管理するサービスアカウントを選択することができます。後者は、異なることを行う、異なるタイプのアクセスを必要とする複数のIDを持つことを計画している場合には良いアイデアですが、そうでなければ、そのアカウントのために秘密鍵ペアを作成する以外に何もする必要がないので、デフォルトを使用する方が簡単です。

> サービスアカウントの種類が決まったら、Google APIs Client Library for Node.jsを使って、DriveやG Suite、その他のGCP以外のGoogle APIに接続するのがベストです。Drive API Node.jsのクイックスタート・チュートリアルを参考にして、"使い方 "を学んでください。Driveのファイルは通常、サービスアカウントではなくユーザーアカウントに属しているため、クイックスタートの認証例では、サービスアカウント認証ではなくOAuthクライアントID（ユーザーアカウント）認証を採用し、ユーザー（Driveの所有者）がアプリに自分のデータへのアクセス許可を与えるように促しています。

> (Cloud Functionsを使用していない場合は、Webアプリやコマンドラインツールを使用していることが多いと思います。そのような場合、service acct authではなくuser acct authを選択すると、エンドユーザー（ドライブのファイル所有者）は、あなたのコードがファイルにアクセスするために、おなじみのOAuth consentダイアログを介して明示的に許可を与えなければなりません。開発者としては、事前にクラウドコンソールでそのOAuth同意画面を設定する必要があります。注：このスクリーンショットは、私のG Suite APIs intro tutorial [Python]から引用しました）。)

> 必要な定型文をサンプルからコピーし、user acct authをservice account authに置き換えます。その方法については、Node.jsクライアントライブラリのドキュメントのサービスアカウントのセクションを参照してください。サービスアカウントのページで、使用するサービスアカウントの秘密鍵ペアを作成し、そのアカウントが持つべき役割や権限を設定することを忘れないでください。作成された鍵ファイルをダウンロードするように促されますので、そのファイルを自分の機能と一緒にアップロードしてください（他の回答で提案されているようにSecret Managerを使用する場合を除く）。クライアントライブラリを使用することで、リクエストヘッダーに何かを貼り付ける必要がなくなります（これも別の回答で提案されています）。しかし、CI/CDサイクルの一環としてGitに鍵ファイルをチェックインすることは、いかなる状況においてもすべきではありません。もしあなたの秘密鍵が漏洩したり公開されたりしたら... もし秘密鍵が公に漏れたり公開されたりしたら、あなたには破滅が訪れるでしょう。


www.DeepL.com/Translator（無料版）で翻訳しました。
