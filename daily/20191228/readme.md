## したい

- async APIの意味
- slackのevents APIをいじって認証
- slackでthreadでresponse
- consoleを非同期的に監視する

## python contextvars

そういえばcontextvarsの使いかたは手に馴染んでいないな。

## monogusa

とりあえずslackから動かせるようにするか。

## python discord

ついでにdiscord botの方も試してみるか。

- https://discordapp.com/developers/applications/

## python slack そもそもbotとslash commandは別

- https://api.slack.com/events
- https://github.com/slackapi/python-slack-events-api
- slash command https://hacknote.jp/archives/39319/
- https://qiita.com/namutaka/items/233a83100c94af033575

## python slack 用のコードをみてみることにしよう

- [slackbot](https://github.com/lins05/slackbot)
- [slacker](https://github.com/os/slacker/)

slackbotはslackerに依存していた。

### botどうやるんだっけ？

- https://snoalot.slack.com/apps/new/A0F7YS25R-bots

できた。

### ところでslackbotのコード微妙じゃない？

- 利用していないimportを残している
- import時に設定を読み込む

ただ、便利なコードが一部存在しているので楽みたいな感じ。一応全体のコードを読んでおくか。

```
bot -> settings
    -> manager
    -> slackclient
    -> dispatcher

bot thread -- keep_alive
bot -- dispatcher.loop
```

- keep aliveは30 * 60秒に一回clientのpingを呼ぶ -> WSのtype:pingを呼ぶだけ
- 概ね処理はdispatcherに渡されてく
- ユーザーの入力をどうやって扱っているかと言うと、rtm_read()のeventを集めている

なるほど。基本的にはコマンドの実行的なものはslackerにまかせて、RTM APIでの情報獲得とかbotの機能をslackbotがやっているのか。うーん。

### 追記

諦めて使うのが費用対効果が良さそう。。

listen_toの実装だけ把握するか

```py
@listen_to('hello$')
def hello_send(message):
    message.send('hello channel!')
```

bot.py

```py
def listen_to(matchstr, flags=0):
    def wrapper(func):
        PluginsManager.commands['listen_to'][
            re.compile(matchstr, flags)] = func
        logger.info('registered listen_to plugin "%s" to "%s"', func.__name__,
                    matchstr)
        return func

    return wrapper
```
