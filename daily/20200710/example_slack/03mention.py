import typing as t
import os
from functools import wraps
from slack import RTMClient
from slack.errors import SlackApiError
from handofcats import as_command


mapping = {"@podhmo": "me", "@me": "podhmo"}
# todo: collect members?


def error_handler(afn: t.Awaitable[t.Callable[..., t.Any]]):
    @wraps(afn)
    async def _do(*args: t.Any, **kwargs: t.Any) -> t.Any:
        try:
            return await afn(*args, **kwargs)
        except SlackApiError as e:
            # You will get a SlackApiError if "ok" is False
            assert e.response["ok"] is False
            assert e.response["error"]  # str like 'invalid_auth', 'channel_not_found'
            print(f"Got an error: {e.response['error']}")

    return _do


profile = None


@RTMClient.run_on(event="open")
async def logined(**payload):
    global profile
    if "data" in payload and "self" in payload["data"]:
        profile = payload["data"]["self"]
        assert "id" in profile  # {"id", "name"}


# https://api.slack.com/events/message
# data/subtype :: message_changed, message_replied, bot_message, 何もなし
# hidden
@RTMClient.run_on(event="message")
@error_handler
async def say_hello(**payload):
    global profile
    if profile is None:
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        return

    print("@@", payload)

    if "data" not in payload:
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        return

    if payload["data"].get("hidden"):
        print("HIDDEN")
        return

    if payload.get("data") and "subtype" in payload["data"]:
        if payload["data"]["subtype"] == "bot_message":  # or "message_replied"
            # see bot id?
            print("****************************************")
            return

    data = payload["data"]
    web_client = payload["web_client"]

    if "text" not in data:
        return

    text = data["text"]
    for alias, name in mapping.items():
        if alias not in text:
            continue

        channel_id = data["channel"]
        thread_ts = data["ts"]
        _ = await web_client.chat_postMessage(
            channel=channel_id,
            text=f"<@{name}> is mentiond in xxx",
            thread_ts=thread_ts,
        )


@as_command
def run():
    import asyncio

    async def _run():
        loop = asyncio.get_running_loop()
        rtm_client = RTMClient(
            token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop
        )
        # rtm_client._event_loop = loop
        # rtm_client._web_client._event_loop = loop
        await rtm_client.start()

    asyncio.run(_run(), debug=True)
