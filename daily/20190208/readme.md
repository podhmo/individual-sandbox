## json har

```
paths
  url
  method
  request
    headers
    query
  response
    status
    message
```

- browserなどの情報いる？
- pagesなどの情報いる？

```
#/log/entries[]/request/method
#/log/entries[]/request/url
#/log/entries[]/request/bodySize  # xxx
#/log/entries[]/request/headers[]
#/log/entries[]/request/queryString
#/log/entries[]/response/status
#/log/entries[]/response/statusText
#/log/entries[]/response/headers[]  # xxx
#/log/entries[]/response/cookies[]  # xxx
#/log/entries[]/response/content/size  # xxx
#/log/entries[]/response/content/text
#/log/entries[]/response/content/mimeType  # or content-Type
#/log/entries[]/response/redirectURL  # xxx
#/log/entries[]/response/bodySize  # xxx
```

自分で調べずにこれが一番便利。

https://github.com/ahmadnassri/har-spec/blob/master/versions/1.3.md

## json harファイルの内容を調べたい

とりあえずhatenaブログの中を覗くか

https://pod.hatenablog.com

- Archive 19-02-08 22-14-35.har

その前に色々バグができた

## swaggerknife 複数形にinflectionを使う

hmm

## swaggerknife json2swagger primitiveなarrayに対応する

## python slackbot

https://api.slack.com/docs/token-types#bot
https://api.slack.com/apps/AG39U250W/install-on-team?

個々までたどり着くのがめんどくさい（主にtoken)

