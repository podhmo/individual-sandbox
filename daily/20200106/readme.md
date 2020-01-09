## ちょっとした興味

- slack上でgithub,gsuiteのアカウントの情報を使う。あれ。それ。
- protocol buffers, gnostic　https://github.com/googleapis/gnostic
- grpcで通信の手軽さを確認
- https://github.com/agronholm/apscheduler の中を読む
- https://developers.google.com/identity/fido


## もう少しだけ丁寧に見ていきたい

- https://stackoverflow.com/questions/50401755/requests-library-with-googleapiclient
- oauth2clientの置き換えどうする？
- 認証のタイプは?

  - OAuth2.0 クライアント ID
  - サービスアカウント キー
  - https://cloud.google.com/docs/authentication/?hl=ja&_ga=2.126156429.-1432081073.1509175144

- サービスアカウントはbotのようなも
- ? 公開データにread onlyではapi key

### CLIでaccess tokenの管理をしたい

- authorize
- revoke
- token info
- list
- use this

### google apiのscopesを一覧したい

- https://developers.google.com/identity/protocols/googlescopes

```
Google Sheets API, v4
Scopes
https://www.googleapis.com/auth/drive	See, edit, create, and delete all of your Google Drive files
https://www.googleapis.com/auth/drive.file	View and manage Google Drive files and folders that you have opened or created with this app
https://www.googleapis.com/auth/drive.readonly	See and download all your Google Drive files
https://www.googleapis.com/auth/spreadsheets	See, edit, create, and delete your spreadsheets in Google Drive
https://www.googleapis.com/auth/spreadsheets.readonly	View your Google Spreadsheets
```

こういう情報を機械的に取れないんだろうか？

- https://github.com/googleapis/google-api-go-client/blob/master/bigquery/v2/bigquery-api.json
- https://github.com/googleapis/google-api-go-client/commit/580a1263e4675cbbc9f1ff7030d35083cc142078#diff-9dd84f33b68940ecef80fba83de14faf

 googleapis-publisher 

### protos

- https://github.com/googleapis/api-common-protos
- https://pypi.org/project/googleapis-common-protos/
- https://github.com/googleapis/googleapis

## python oauthlib

- そもそもgooauth-auth-oauthlib自体はそこまでoauthlibとの関わりがあるわけではないかも？
- google api用のcredentialsを取るためのものっぽい

flask-danceのほうがあってるかも。

### 便利そうな奴ら

- https://github.com/singingwolfboy/flask-dance
- https://github.com/lepture/authlib
- https://github.com/authlib/loginpass

## python googleapi

googleapi関係のパッケージ多すぎない？

```console
$ (pip freeze | grep google; pip freeze oauth | grep oauth) | sort -u
google-api-python-client==1.7.11
google-auth-httplib2==0.0.3
google-auth-oauthlib==0.4.1
google-auth==1.10.0
oauth2client==4.1.3
oauthlib==3.1.0
requests-oauthlib==1.3.0
```

### oauth2client is deprecated

https://pypi.org/project/oauth2client/

> Note: oauth2client is now deprecated. No more features will be added to the
>    libraries and the core team is turning down support. We recommend you use [google-auth](https://google-auth.readthedocs.io/en/latest/) and [oauthlib](https://oauthlib.readthedocs.io/en/latest/). 


理由はこのあたり

https://google-auth.readthedocs.io/en/latest/oauth2client-deprecation.html


### spread sheetを触るのによくみるやつ gspread

明示的に依存は入っていない。

```console
$ pipdeptree -p gspread
gspread==3.1.0
  - requests [required: >=2.2.1, installed: 2.22.0]
    - certifi [required: >=2017.4.17, installed: 2019.11.28]
    - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
    - idna [required: >=2.5,<2.9, installed: 2.8]
    - urllib3 [required: >=1.21.1,<1.26,!=1.25.1,!=1.25.0, installed: 1.25.7]
```

しかしドキュメントはoauth2clientを使っている。

- https://gspread.readthedocs.io/en/latest/oauth2.html#oauth-credentials

なるほどまだ。PR

- https://github.com/burnash/gspread/pull/711

### googleの方の認証の方のドキュメントは？


- https://cloud.google.com/docs/authentication/?hl=ja
- [service account key](https://cloud.google.com/docs/authentication/production?hl=ja)
- [oauth2 client credential](https://cloud.google.com/docs/authentication/end-user?hl=ja)
- [api key](https://cloud.google.com/docs/authentication/api-keys?hl=ja)

> 有効な認証情報タイプには、API キー、OAuth 2.0 クライアント資格情報、サービス アカウント キーがあります

#### service account key

コード的にはこの辺を見れば良い。

- https://github.com/GoogleCloudPlatform/python-docs-samples/tree/master/auth

```python
# pip install --upgrade google-cloud-storage

def explicit():
    from google.cloud import storage

    # Explicitly use service account credentials by specifying the private key
    # file.
    storage_client = storage.Client.from_service_account_json(
        'service_account.json')

    # Make an authenticated API request
    buckets = list(storage_client.list_buckets())
    print(buckets)
```

implicit

```python
# set GOOGLE_APPLICATION_CREDENTIALS
# export = "/home/user/Downloads/[FILE_NAME].json"

def implicit():
    from google.cloud import storage

    # If you don't specify credentials when constructing the client, the
    # client library will look for credentials in the environment.
    storage_client = storage.Client()

    # Make an authenticated API request
    buckets = list(storage_client.list_buckets())
    print(buckets)
```


#### oauth2 client

```python
# need pip install --upgrade google-auth-oauthlib

from google_auth_oauthlib import flow

# TODO: Uncomment the line below to set the `launch_browser` variable.
# launch_browser = True
#
# The `launch_browser` boolean variable indicates if a local server is used
# as the callback URL in the auth flow. A value of `True` is recommended,
# but a local server does not work if accessing the application remotely,
# such as over SSH or from a remote Jupyter notebook.

appflow = flow.InstalledAppFlow.from_client_secrets_file(
    'client_secrets.json',
    scopes=['https://www.googleapis.com/auth/bigquery'])

if launch_browser:
    appflow.run_local_server()
else:
    appflow.run_console()

credentials = appflow.credentials
```


でこう使う。

```python
from google.cloud import bigquery

# TODO: Uncomment the line below to set the `project` variable.
# project = 'user-project-id'
#
# The `project` variable defines the project to be billed for query
# processing. The user must have the bigquery.jobs.create permission on
# this project to run a query. See:
# https://cloud.google.com/bigquery/docs/access-control#permissions

client = bigquery.Client(project=project, credentials=credentials)

query_string = """SELECT name, SUM(number) as total
FROM `bigquery-public-data.usa_names.usa_1910_current`
WHERE name = 'William'
GROUP BY name;
"""
query_job = client.query(query_string)

# Print the results.
for row in query_job.result():  # Wait for the job to complete.
    print("{}: {}".format(row['name'], row['total']))
```

### spreadsheet?

なんか素直にドキュメント読めばよかったのかも。oauth2 clientの場合も載ってるじゃん。

- https://developers.google.com/sheets/api/quickstart/python

```python
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/spreadsheets.readonly']

# The ID and range of a sample spreadsheet.
SAMPLE_SPREADSHEET_ID = '1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms'
SAMPLE_RANGE_NAME = 'Class Data!A2:E'

def main():
    """Shows basic usage of the Sheets API.
    Prints values from a sample spreadsheet.
    """
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    service = build('sheets', 'v4', credentials=creds)

    # Call the Sheets API
    sheet = service.spreadsheets()
    result = sheet.values().get(spreadsheetId=SAMPLE_SPREADSHEET_ID,
                                range=SAMPLE_RANGE_NAME).execute()
    values = result.get('values', [])

    if not values:
        print('No data found.')
    else:
        print('Name, Major:')
        for row in values:
            # Print columns A and E, which correspond to indices 0 and 4.
            print('%s, %s' % (row[0], row[4]))

if __name__ == '__main__':
    main()
```

### なんか管理するツールが用意されてる？

```console
$ python -m google_auth_oauthlib.tool --help
Usage: __main__.py [OPTIONS]

  Command-line tool for obtaining authorization and credentials from a user.

  This tool uses the OAuth 2.0 Authorization Code grant as described in
  section 1.3.1 of RFC6749:
  https://tools.ietf.org/html/rfc6749#section-1.3.1

  This tool is intended for assist developers in obtaining credentials for
  testing applications where it may not be possible or easy to run a
  complete OAuth 2.0 authorization flow, especially in the case of code
  samples or embedded devices without input / display capabilities.

  This is not intended for production use where a combination of companion
  and on-device applications should complete the OAuth 2.0 authorization
  flow to get authorization from the users.

Options:
  --client-secrets <client_secret_json_file>
                                  Path to OAuth2 client secret JSON file.
                                  [required]
  --scope <oauth2 scope>          API scopes to authorize access for.
                                  [required]
  --save                          Save the credentials to file.  [default:
                                  False]
  --credentials <oauth2_credentials>
                                  Path to store OAuth2 credentials.  [default:
                                  /Users/nao/Library/Application
                                  Support/google-oauthlib-
                                  tool/credentials.json]
  --headless                      Run a console based flow.  [default: False]
  --help                          Show this message and exit.
```

### 自分で作りたいなら

- https://github.com/googleapis/googleapis


## python manytables

そういえば昔に作っていた

- https://github.com/podhmo/manytables

### 追記

これはもともとspreadsheetからはじめて、それを手元のtsvにコピーしてきてから書き換える。
その後またspreadsheetに同期をするみたいなやつっぽい。
ちょっとイメージしていたものとは違いそう。

## python rpcもちょっとだけ見ていきたい

- https://aiozmq.readthedocs.io/
- https://pythonhosted.org/asyncrpc/

## 追記

どうもasyncrpcはメンテされてなさそうだ。ダメそう。

## 追記

aiozmqもだめっぽい。asyncio.asyncで死ぬ。
(時代について行けてない)


## python tiny task queue (job queue)

- https://github.com/agronholm/apscheduler
- https://github.com/coleifer/huey
- https://github.com/samuelcolvin/arq

ちょっといろいろ試してみよう。
