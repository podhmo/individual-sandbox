# slackbot

## とりあえずどんなメッセージが取得できるか見る。

listen toで取り出したメッセージを表示

listen_to
```
(Pdb) pp vars(message)
{'_body': {'blocks': [{'block_id': 'd79F',
                       'elements': [{'elements': [{'text': 'ho ho',
                                                   'type': 'text'}],
                                     'type': 'rich_text_section'}],
                       'type': 'rich_text'}],
           'channel': 'C02M87T2E',
           'client_msg_id': '84046edc-eaf4-4517-ae10-68b67bbe15ba',
           'event_ts': '1581485306.000200',
           'source_team': 'T02M87T1A',
           'suppress_notification': False,
           'team': 'T02M87T1A',
           'text': 'ho ho',
           'ts': '1581485306.000200',
           'type': 'message',
           'user': 'U02M87T1J',
           'user_team': 'T02M87T1A'},
 '_client': <slackbot.slackclient.SlackClient object at 0x10e0ddf70>,
 '_plugins': <slackbot.manager.PluginsManager object at 0x10ec1dd00>}
```

この辺の例はAPI documentに書いてあった気がする

- https://api.slack.com/events/message

## slackbotでreactionは受け取れる？

テキトーにdumpしてみた結果

- [./messages.json](./messages.json)

reaction_addとかが取れれば良い。

- https://api.slack.com/events/reaction_added
- https://api.slack.com/events/reaction_removed

だめっぽい。

https://github.com/lins05/slackbot/blob/7657dd144de2355b7e29a1670549d32de2ebb42b/slackbot/dispatcher.py#L140-L154

なるほど。
