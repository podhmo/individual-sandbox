setup

```console
$ pip install -r requirements.txt
```

and creating bot app in slack app page, and activating it.

```
# adding bot token e.g. xoxb-xxxxxxxxxx-xxxxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxx
$ edit .token.txt
```

and run app

```
$ make 00
```

sample app is like a below.

```python
from slackbot.bot import Bot


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    main()
```

### setting

if you want to use customize setting, edit slackbot_settings.py

### adding your new feature

if you want to add new feature yourown, adding the file in plugins directory.
(why do so that? see slackbot_setting.py's PLUGINS setting)
