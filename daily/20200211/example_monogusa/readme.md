# monongusa

以下のことが気になっている

- 反応を定義したい
- コード生成を主体にしたい

今回は概ね前者。

## 反応を定義したい

CLIのコマンドではなくchat botとしての機能を考えたときに、メッセージを監視しての反応という操作が定義できてほしい。

例えばこの種の動作としては以下のようなものがある。

- メッセージ中のprivateなURLを発見したら、情報を取得し展開する
- メッセージ中の特定の表現を監視し、記録する（たとえばreactionとしてのemoji）
- (それぞれのチャット上の生の表現をどう扱うか？が別途問題になる)
- (添付ファイルとかその付近も考えたいところ(?)。これはS3などに保存してlinkとかにしたい)
- (認証や権限をどうするという話も出てくる？)

これを手軽に試せるとはどういうことだろうか？

### 調査

現状は以下の様な方針でやれば良いと思っている

- 特定のstreamを引数としてメッセージをシミュレートする(e.g. stdin)

一応discord.pyとslackbotの実装を覗いて見るのが良いかもしれない。

- https://github.com/lins05/slackbot
- https://github.com/Rapptz/discord.py

### slackbot

https://github.com/lins05/slackbot#create-plugins

slackbotはDMを読むかchannel上のmessageを読むかで分かれるものの概ねこんな感じ。
(ちなみにrespond_toがDMやそのbotへのmention。listen_toはbotの可視範囲内でのmessageの監視)

これになぞらえるならlisten_toだけが定義できれば良いのでは？

```python
@listen_to('Can someone help me?')
def help(message):
    # Message is replied to the sender (prefixed with @user)
    message.reply('Yes, I can!')

    # Message is sent on the channel
    message.send('I can help everybody!')

    # Start a thread on the original message
    message.reply("Here's a threaded reply", in_thread=True)


@respond_to('Give me (.*)')
def giveme(message, something):
    message.reply('Here is {}'.format(something))
```

実装は。。(見る必要ある？）

#### 追記

discord.pyを覗いて見たあとで考えると、`on_message()`のようなものを直接利用する方法を調べておくのは良い事かもしれない。

内部的には、↑のデコレーターはPluginMangerというオブジェクトの**クラス**に関数を登録していく機能な模様。

そしてManagerのところで、登録していた正規表現にマッチするかを調べているらしい。

slackbot/manager.py

```python
class PluginsManager(object):
    def __init__(self): # こういうコード不要なんだよなー
        pass

    commands = {
        'respond_to': {},
        'listen_to': {},
        'default_reply': {}
    }

...

    def get_plugins(self, category, text):
        has_matching_plugin = False
        if text is None:
            text = ''
        for matcher in self.commands[category]:
            m = matcher.search(text)
            if m:
                has_matching_plugin = True
                yield self.commands[category][matcher], to_utf8(m.groups())

        if not has_matching_plugin:
            yield None, None
```

実際の動作はどこで行われているかというと、DispatcherというオブジェクトがRTMAPIの通信を呼んでいて、ここで先程の`get_plugins()`が使われている。

dispatcher.py

```python
class MessageDispatcher(object):
    def __init__(self, slackclient, plugins, errors_to):
        self._client = slackclient
        self._pool = WorkerPool(self.dispatch_msg) # この内部でget_plugins()などでdispatch
...
```

WorkerPool自体はutilsにあるqueue.Queueを監視するthreadを動かしておく素朴な実装。

utils.py

```python
class WorkerPool(object):
    def __init__(self, func, nworker=10):
        self.nworker = nworker
        self.func = func
        self.queue = queue.Queue()

    def start(self):
        for __ in range(self.nworker):
            _thread.start_new_thread(self.do_work, tuple())

    def add_task(self, msg):
        self.queue.put(msg)

    def do_work(self):
        while True:
            msg = self.queue.get()
            self.func(msg)
```

それじゃこのWorkerPoolの内部のqueueにどこでメッセージが送られているかというと、Dispatcherの`loop()`。

dispatcher.py

```python
class MessageDispatcher(object):
...
    def loop(self):
        while True:
            events = self._client.rtm_read()
            for event in events:
                event_type = event.get('type')
                if event_type == 'message':
                    self._on_new_message(event)
                elif event_type in ['channel_created', 'channel_rename',
                                    'group_joined', 'group_rename',
                                    'im_created']:
                    channel = [event['channel']]
                    self._client.parse_channel_data(channel)
                elif event_type in ['team_join', 'user_change']:
                    user = [event['user']]
                    self._client.parse_user_data(user)
            time.sleep(1)
```

単純にまとめるとこういうこと。

```python
def loop(client):
    for msg in client.rtm_read():
        for rx, fn in handlers:
            m = rx.match(msg.text)
            if m is not None:
                fn(msg)
```

まぁ素直な感じ。なのでもともと想定していた通り以下の様な状態

- 何らかのstream (iterator) を購読してあげれば良い
- 何かのオブジェクトでwrapされている部分はどう扱おう？ (fakeやadapterは修羅の道だと思う)

そしてdiscord.pyを覗いて感じた危惧は正解で

- slackbotを直接使うと、正規表現でのマッチ以外書きづらい。

ところでemojiなどでのリアクションも `rtm_read()`に乗ってくるのだろうか？

- https://api.slack.com/events/reaction_added
- https://api.slack.com/events/reaction_removed

### discord.py

https://github.com/Rapptz/discord.py#quick-example

低レベルな記述では `on_message()` 上で自分で反応を記述する。

```python
import discord

class MyClient(discord.Client):
    async def on_message(self, message):
        # don't respond to ourselves
        if message.author == self.user:
            return

        if message.content == 'ping':
            await message.channel.send('pong')
```

(`on_ready()`に類するものって必要？)


真面目に調べるならこの辺のevents関連を監視するという形っぽいな。

- https://discordpy.readthedocs.io/en/latest/api.html#discord-api-events

ちなみに↑のようなコードを短縮するようなデコレーターも用意されているみたい。

```python

@client.event
async def on_message(message):
    if message.author == client.user:
        return

    if message.content.startswith('$hello'):
        await message.channel.send('Hello!')
```

#### 追記

ついでにdiscord.pyでのメッセージの伝達経路を調べてみるか。

基本的にはdiscord.Clientがroot。以下２つの中に注力して見ていけば良さそう。

- `__init__()`
- `run()`

これだけだとinstance variablesが見れないので中のコードを覗くか。

```console
$ pyinspect inspect discord.client:Client
discord.client:Client <- builtins:object
    [method, OVERRIDE] __init__(self, *, loop=None, **options)
        [method] _handle_ready(self)
        [method] dispatch(self, event, *args, **kwargs)
            [method] _schedule_event(self, coro, event_name, *args, **kwargs)
                [method] _run_event(self, coro, event_name, *args, **kwargs)
                    [method] on_error(self, event_method, *args, **kwargs)
        [method] _chunker(self, guild)
        [method] _syncer(self, guilds)
    [property] activity
    [method] application_info(self)
    [property] cached_messages
    [method] change_presence(self, *, activity=None, status=None, afk=False)
    [method] clear(self)
    [method] create_guild(self, name, region=None, icon=None)
    [method] delete_invite(self, invite)
    [property] emojis
    [method] event(self, coro)
    [method] fetch_channel(self, channel_id)
        [property] user
        [method] get_guild(self, id)
    [method] fetch_guild(self, guild_id)
    [method] fetch_guilds(self, *, limit=100, before=None, after=None)
    [method] fetch_invite(self, url, *, with_counts=True)
    [method] fetch_user(self, user_id)
    [method] fetch_user_profile(self, user_id)
    [method] fetch_webhook(self, webhook_id)
    [method] fetch_widget(self, guild_id)
    [method] get_all_channels(self)
        [property] guilds
    [method] get_all_members(self)
        [property] guilds
    [method] get_channel(self, id)
    [method] get_emoji(self, id)
    [method] get_user(self, id)
    [method] is_ready(self)
    [property] latency
    [method] logout(self)
        [method] close(self)
            [property] voice_clients
    [property] private_channels
    [method] request_offline_members(self, *guilds)
    [method] run(self, *args, **kwargs)
        [method] start(self, *args, **kwargs)
            [method] login(self, token, *, bot=True)
            [method] connect(self, *, reconnect=True)
                [method] is_closed(self)
                [method] _connect(self)
                    [method] dispatch(self, event, *args, **kwargs)
                        [method] _schedule_event(self, coro, event_name, *args, **kwargs)
                            [method] _run_event(self, coro, event_name, *args, **kwargs)
                                [method] on_error(self, event_method, *args, **kwargs)
                [method] dispatch(self, event, *args, **kwargs)
                    [method] _schedule_event(self, coro, event_name, *args, **kwargs)
                        [method] _run_event(self, coro, event_name, *args, **kwargs)
                            [method] on_error(self, event_method, *args, **kwargs)
                [method] close(self)
                    [property] voice_clients
        [method] close(self)
            [property] voice_clients
    [property] users
    [method] wait_for(self, event, *, check=None, timeout=None)
    [method] wait_until_ready(self)
```

wsとloopとconnectionあたりを読んでいけば良さそう。

client.py

```python
class Client:
"""
...
    Attributes
    -----------
    ws
        The websocket gateway the client is currently connected to. Could be ``None``.
    loop: :class:`asyncio.AbstractEventLoop`
        The event loop that the client uses for HTTP requests and websocket operations.
"""
    def __init__(self, *, loop=None, **options):
        self.ws = None
        self.loop = asyncio.get_event_loop() if loop is None else loop
        self._listeners = {}
        self.shard_id = options.get('shard_id')
        self.shard_count = options.get('shard_count')

        connector = options.pop('connector', None)
        proxy = options.pop('proxy', None)
        proxy_auth = options.pop('proxy_auth', None)
        self.http = HTTPClient(connector, proxy=proxy, proxy_auth=proxy_auth, loop=self.loop)

        self._handlers = {
            'ready': self._handle_ready
        }

        self._connection = ConnectionState(dispatch=self.dispatch, chunker=self._chunker, handlers=self._handlers,
                                           syncer=self._syncer, http=self.http, loop=self.loop, **options)

        self._connection.shard_count = self.shard_count
        self._closed = False
        self._ready = asyncio.Event(loop=self.loop)
        self._connection._get_websocket = lambda g: self.ws

        if VoiceClient.warn_nacl:
            VoiceClient.warn_nacl = False
            log.warning("PyNaCl is not installed, voice will NOT be supported")
```

connection部分が概ね通信のやり取りを気にしていそう（あとは受け取ったeventをlistenersなりに渡しているのだと思う）。

大きいけれどparseしたりなどしているだけかも。

```console
$ pyinspect inspect discord.state:ConnectionState
discord.state:ConnectionState <- builtins:object
    [method, OVERRIDE] __init__(self, *, dispatch, chunker, handlers, syncer, http, loop, **options)
        [method] clear(self)
    [method] _add_voice_client(self, guild_id, voice)
    [method] _get_private_channel_by_user(self, user_id)
    [method] _remove_voice_client(self, guild_id)
    [method] _update_references(self, ws)
        [property] voice_clients
    [method] add_dm_channel(self, data)
        [method] _add_private_channel(self, channel)
    [method] create_message(self, *, channel, data)
    [property] emojis
    [method] get_emoji(self, emoji_id)
    [method] get_reaction_emoji(self, data)
    [method] parse_channel_create(self, data)
        [method] _get_guild(self, guild_id)
        [method] _get_private_channel(self, channel_id)
        [method] _add_private_channel(self, channel)
    [method] parse_channel_delete(self, data)
        [method] _get_guild(self, guild_id)
        [method] _get_private_channel(self, channel_id)
        [method] _remove_private_channel(self, channel)
    [method] parse_channel_pins_update(self, data)
        [method] get_channel(self, id)
            [property] guilds
            [method] _get_private_channel(self, channel_id)
    [method] parse_channel_recipient_add(self, data)
        [method] _get_private_channel(self, channel_id)
        [method] store_user(self, data)
    [method] parse_channel_recipient_remove(self, data)
        [method] _get_private_channel(self, channel_id)
        [method] store_user(self, data)
    [method] parse_channel_update(self, data)
        [method] _get_guild(self, guild_id)
        [method] _get_private_channel(self, channel_id)
    [method] parse_guild_ban_add(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_ban_remove(self, data)
        [method] _get_guild(self, guild_id)
        [method] store_user(self, data)
    [method] parse_guild_create(self, data)
        [method] _get_create_guild(self, data)
            [method] _add_guild_from_data(self, guild)
                [method] _add_guild(self, guild)
            [method] _get_guild(self, guild_id)
        [method] _chunk_and_dispatch(self, guild, unavailable)
            [method] chunks_needed(self, guild)
                [method] receive_chunk(self, guild_id)
    [method] parse_guild_delete(self, data)
        [method] _get_guild(self, guild_id)
        [method] _remove_guild(self, guild)
    [method] parse_guild_emojis_update(self, data)
        [method] _get_guild(self, guild_id)
        [method] store_emoji(self, guild, data)
    [method] parse_guild_integrations_update(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_member_add(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_member_remove(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_member_update(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_members_chunk(self, data)
        [method] _get_guild(self, guild_id)
        [method] process_listeners(self, listener_type, argument, result)
    [method] parse_guild_role_create(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_role_delete(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_role_update(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_sync(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_guild_update(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_message_create(self, data)
        [method] _get_guild_channel(self, data)
            [method] _get_guild(self, guild_id)
            [method] get_channel(self, id)
                [property] guilds
                [method] _get_private_channel(self, channel_id)
    [method] parse_message_delete(self, data)
        [method] _get_message(self, msg_id)
    [method] parse_message_delete_bulk(self, data)
    [method] parse_message_reaction_add(self, data)
        [method] _get_message(self, msg_id)
        [method] _upgrade_partial_emoji(self, emoji)
        [method] _get_reaction_user(self, channel, user_id)
            [method] get_user(self, id)
    [method] parse_message_reaction_remove(self, data)
        [method] _get_message(self, msg_id)
        [method] _upgrade_partial_emoji(self, emoji)
        [method] _get_reaction_user(self, channel, user_id)
            [method] get_user(self, id)
    [method] parse_message_reaction_remove_all(self, data)
        [method] _get_message(self, msg_id)
    [method] parse_message_update(self, data)
        [method] _get_message(self, msg_id)
    [method] parse_presence_update(self, data)
        [method] _get_guild(self, guild_id)
    [method] parse_ready(self, data)
        [method] clear(self)
        [method] _add_guild_from_data(self, guild)
            [method] _add_guild(self, guild)
        [method] _add_private_channel(self, channel)
        [method] _delay_ready(self)
            [method] call_handlers(self, key, *args, **kwargs)
            [method] request_offline_members(self, guilds)
                [method] chunks_needed(self, guild)
                    [method] receive_chunk(self, guild_id)
            [property] guilds
    [method] parse_relationship_add(self, data)
    [method] parse_relationship_remove(self, data)
    [method] parse_resumed(self, data)
    [method] parse_typing_start(self, data)
        [method] _get_guild_channel(self, data)
            [method] _get_guild(self, guild_id)
            [method] get_channel(self, id)
                [property] guilds
                [method] _get_private_channel(self, channel_id)
    [method] parse_user_update(self, data)
    [method] parse_voice_server_update(self, data)
        [method] _get_voice_client(self, guild_id)
    [method] parse_voice_state_update(self, data)
        [method] _get_guild(self, guild_id)
        [method] _get_voice_client(self, guild_id)
    [method] parse_webhooks_update(self, data)
        [method] get_channel(self, id)
            [property] guilds
            [method] _get_private_channel(self, channel_id)
    [property] private_channels
    [property] self_id
```

真面目にclient側の動作を見てみると以下の様になっている。（今度は`run()`を覗いていく）

1. run()実行。
2. run()はloopの中でstart()を実行
3. start()はlogin(),connect()を実行
4. connect()は例外とかいろいろwrapしているけれど、実質_connect()

そういうわけで `_connect()` を覗く。

client.py

```python
class Client:
...

    async def _connect(self):
        coro = DiscordWebSocket.from_client(self, shard_id=self.shard_id)
        self.ws = await asyncio.wait_for(coro, timeout=180.0, loop=self.loop)
        while True:
            try:
                await self.ws.poll_event()
            except ResumeWebSocket:
                log.info('Got a request to RESUME the websocket.')
                self.dispatch('disconnect')
                coro = DiscordWebSocket.from_client(self, shard_id=self.shard_id, session=self.ws.session_id,
                                                    sequence=self.ws.sequence, resume=True)
                self.ws = await asyncio.wait_for(coro, timeout=180.0, loop=self.loop)
```

`ws.poll_event()`が本体のよう。ここちょっと複雑かもだけれど。まぁ基本的にはwebsocketsライブラリのラッパーくらいの感覚で良い。気にするべきは先程出てきた以下のメソッド。

- `from_client()` classmethod
- `poll_event()`

```console
$ pyinspect inspect discord.gateway:DiscordWebSocket
discord.gateway:DiscordWebSocket <- websockets.client:WebSocketClientProtocol <- websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, *args, **kwargs)
    [method] change_presence(self, *, activity=None, status=None, afk=False, since=0.0)
        [method, OVERRIDE] send(self, data)
    [method, OVERRIDE] close_connection(self, *args, **kwargs)
    [property] latency
    [method] poll_event(self)
        [method] received_message(self, msg)
            [method, OVERRIDE] close(self, code=1000, reason='')
            [method] send_as_json(self, data)
                [method, OVERRIDE] send(self, data)
                [method] _can_handle_close(self, code)
            [method] identify(self)
                [method] send_as_json(self, data)
                    [method, OVERRIDE] send(self, data)
                    [method] _can_handle_close(self, code)
        [method] _can_handle_close(self, code)
    [method] request_sync(self, guild_ids)
        [method] send_as_json(self, data)
            [method, OVERRIDE] send(self, data)
            [method] _can_handle_close(self, code)
    [method] resume(self)
        [method] send_as_json(self, data)
            [method, OVERRIDE] send(self, data)
            [method] _can_handle_close(self, code)
    [method] voice_state(self, guild_id, channel_id, self_mute=False, self_deaf=False)
        [method] send_as_json(self, data)
            [method, OVERRIDE] send(self, data)
            [method] _can_handle_close(self, code)
    [method] wait_for(self, event, predicate, result=None)
    [class method] from_client(client, *, shard_id=None, session=None, sequence=None, resume=False)

websockets.client:WebSocketClientProtocol <- websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, *, origin=None, extensions=None, subprotocols=None, extra_headers=None, **kwds)
    [method] handshake(self, wsuri, origin=None, available_extensions=None, available_subprotocols=None, extra_headers=None)
        [static method] process_extensions(headers, available_extensions)
        [static method] process_subprotocol(headers, available_subprotocols)
        [method] write_http_request(self, path, headers)
        [method] read_http_response(self)

websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __aiter__(self)
        [method] recv(self)
            [method] ensure_open(self)
    [method, OVERRIDE] __init__(self, *, host=None, port=None, secure=None, timeout=10, max_size=1048576, max_queue=32, read_limit=65536, write_limit=65536, loop=None, legacy_recv=False)
        [method] client_connected(self, reader, writer)
    [method] close(self, code=1000, reason='')
        [method] fail_connection(self, code=1006, reason='')
            [method] close_connection(self)
                [method] wait_for_connection_lost(self)
        [method] write_close_frame(self, data=b'')
            [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                [method] writer_is_closing(self)
                [method] fail_connection(self, code=1006, reason='')
                    [method] close_connection(self)
                        [method] wait_for_connection_lost(self)
                [method] ensure_open(self)
    [property] closed
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] connection_made(self, transport)
    [method] connection_open(self)
        [method] transfer_data(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] read_message(self)
                [method] read_data_frame(self, max_size)
                    [method] read_frame(self, max_size)
                    [method] write_close_frame(self, data=b'')
                        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
                    [method] pong(self, data=b'')
                        [method] ensure_open(self)
                        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
        [method] close_connection(self)
            [method] wait_for_connection_lost(self)
    [method, OVERRIDE] eof_received(self)
    [property] local_address
    [property] open
    [method] ping(self, data=None)
        [method] ensure_open(self)
        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
            [method] writer_is_closing(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] ensure_open(self)
    [property] remote_address
    [method] send(self, data)
        [method] ensure_open(self)
        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
            [method] writer_is_closing(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] ensure_open(self)

asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __del__(self)
    [method, OVERRIDE] __init__(self, stream_reader, client_connected_cb=None, loop=None)
    [method, OVERRIDE] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_lost(self, exc)
        [property] _stream_reader
    [method, OVERRIDE] connection_made(self, transport)
        [property] _stream_reader
    [method, OVERRIDE] data_received(self, data)
        [property] _stream_reader
    [method, OVERRIDE] eof_received(self)
        [property] _stream_reader

asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, loop=None)
    [method] _drain_helper(self)
    [method] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] pause_writing(self)
    [method, OVERRIDE] resume_writing(self)

asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] data_received(self, data)
    [method] eof_received(self)

asyncio.protocols:BaseProtocol <- builtins:object
    [method] connection_lost(self, exc)
    [method] connection_made(self, transport)
    [method] pause_writing(self)
    [method] resume_writing(self)
```

というわけで `poll_event()`を覗いていく。難しいか？というとそうでもなくって内部の`receive_message()`の中で `_dispatch(event name)`が呼ばれていったタイミングで色々対応するeventのlistenerが反応する仕組み。

gateway.py

```python
class DiscordWebSocket(websockets.client.WebSocketClientProtocol):
...

    async def poll_event(self):
        """Polls for a DISPATCH event and handles the general gateway loop.

        Raises
        ------
        ConnectionClosed
            The websocket connection was terminated for unhandled reasons.
        """
        try:
            msg = await self.recv()
            await self.received_message(msg)
        except websockets.exceptions.ConnectionClosed as exc:
            if self._can_handle_close(exc.code):
                log.info('Websocket closed with %s (%s), attempting a reconnect.', exc.code, exc.reason)
                raise ResumeWebSocket(self.shard_id) from exc
            else:
                log.info('Websocket closed with %s (%s), cannot reconnect.', exc.code, exc.reason)
                raise ConnectionClosed(exc, shard_id=self.shard_id) from exc


    async def received_message(self, msg):
        self._dispatch('socket_raw_receive', msg)

        if type(msg) is bytes:
            self._buffer.extend(msg)

            if len(msg) >= 4:
                if msg[-4:] == b'\x00\x00\xff\xff':
                    msg = self._zlib.decompress(self._buffer)
                    msg = msg.decode('utf-8')
                    self._buffer = bytearray()
                else:
                    return
            else:
                return

        msg = json.loads(msg)

        log.debug('For Shard ID %s: WebSocket Event: %s', self.shard_id, msg)
        self._dispatch('socket_response', msg)

        op = msg.get('op')
        data = msg.get('d')
        seq = msg.get('s')
        if seq is not None:
            self.sequence = seq

        if op != self.DISPATCH:
            if op == self.RECONNECT:
                # "reconnect" can only be handled by the Client
                # so we terminate our connection and raise an
                # internal exception signalling to reconnect.
                log.info('Received RECONNECT opcode.')
                await self.close()
                raise ResumeWebSocket(self.shard_id)

            if op == self.HEARTBEAT_ACK:
                self._keep_alive.ack()
                return

            if op == self.HEARTBEAT:
                beat = self._keep_alive.get_payload()
                await self.send_as_json(beat)
                return

            if op == self.HELLO:
                interval = data['heartbeat_interval'] / 1000.0
                self._keep_alive = KeepAliveHandler(ws=self, interval=interval, shard_id=self.shard_id)
                # send a heartbeat immediately
                await self.send_as_json(self._keep_alive.get_payload())
                self._keep_alive.start()
                return

            if op == self.INVALIDATE_SESSION:
                if data is True:
                    await asyncio.sleep(5.0, loop=self.loop)
                    await self.close()
                    raise ResumeWebSocket(self.shard_id)

                self.sequence = None
                self.session_id = None
                log.info('Shard ID %s session has been invalidated.', self.shard_id)
                await self.identify()
                return

            log.warning('Unknown OP code %s.', op)
            return

        event = msg.get('t')

        if event == 'READY':
            self._trace = trace = data.get('_trace', [])
            self.sequence = msg['s']
            self.session_id = data['session_id']
            log.info('Shard ID %s has connected to Gateway: %s (Session ID: %s).',
                     self.shard_id, ', '.join(trace), self.session_id)

        elif event == 'RESUMED':
            self._trace = trace = data.get('_trace', [])
            log.info('Shard ID %s has successfully RESUMED session %s under trace %s.',
                     self.shard_id, self.session_id, ', '.join(trace))

        try:
            func = self._discord_parsers[event]
        except KeyError:
            log.warning('Unknown event %s.', event)
        else:
            func(data)

        # remove the dispatched listeners
        removed = []
        for index, entry in enumerate(self._dispatch_listeners):
            if entry.event != event:
                continue

            future = entry.future
            if future.cancelled():
                removed.append(index)
                continue

            try:
                valid = entry.predicate(data)
            except Exception as exc:
                future.set_exception(exc)
                removed.append(index)
            else:
                if valid:
                    ret = data if entry.result is None else entry.result(data)
                    future.set_result(ret)
                    removed.append(index)

        for index in reversed(removed):
            del self._dispatch_listeners[index]
```

ちなみに`_dispatch()` で何が呼ばれるかというと、それは先程出てきた `from_client()` のコードが参考になる。

gateway.py

```python
class DiscordWebSocket(websockets.client.WebSocketClientProtocol):
...

    @classmethod
    async def from_client(cls, client, *, shard_id=None, session=None, sequence=None, resume=False):
...

        gateway = await client.http.get_gateway()
        ws = await websockets.connect(gateway, loop=client.loop, klass=cls, compression=None)

        # dynamically add attributes needed
        ws.token = client.http.token
        ws._connection = client._connection
        ws._discord_parsers = client._connection.parsers
        ws._dispatch = client.dispatch
```

そんなわけでclient側の`dispatch()`が良い感じで呼ばれることになる。
じゃぁclientの`dispatch()`がどういう定義かというと、`on_ + <event name>`のメソッドを呼ぶ感じ。

client.py

```python
class Client:

...

    def dispatch(self, event, *args, **kwargs):
        log.debug('Dispatching event %s', event)
        method = 'on_' + event

        listeners = self._listeners.get(event)
        if listeners:
            removed = []
            for i, (future, condition) in enumerate(listeners):
                if future.cancelled():
                    removed.append(i)
                    continue

                try:
                    result = condition(*args)
                except Exception as exc:
                    future.set_exception(exc)
                    removed.append(i)
                else:
                    if result:
                        if len(args) == 0:
                            future.set_result(None)
                        elif len(args) == 1:
                            future.set_result(args[0])
                        else:
                            future.set_result(args)
                        removed.append(i)

            if len(removed) == len(listeners):
                self._listeners.pop(event)
            else:
                for idx in reversed(removed):
                    del listeners[idx]

        try:
            coro = getattr(self, method)
        except AttributeError:
            pass
        else:
            self._schedule_event(coro, method, *args, **kwargs)
```

細かい話をすると、自分自身のメソッドに紐付けられたものを呼ぶわけなので、同じイベントに反応するメソッドは一つだけの模様。
