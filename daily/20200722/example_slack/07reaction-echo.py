import typing as t
import os
import asyncio
from slack import RTMClient
from slack import WebClient


@RTMClient.run_on(event="message")
async def say_hello(**payload):
    """"Hello" を含んだメッセージに反応して "Hi @<user名>" を返す """
    web_client: WebClient = payload["web_client"]
    data = payload.get("data") or {}
    if "text" in data and "Hello" in data.get("text", []):
        channel_id = data["channel"]
        thread_ts = data["ts"]
        user = data["user"]

        await web_client.chat_postMessage(
            channel=channel_id, text=f"Hi <@{user}>!", thread_ts=thread_ts
        )


@RTMClient.run_on(event="reaction_added")
async def echo_emoji(
    *, web_client: WebClient, rtm_client: RTMClient, data: t.Dict[str, t.Any], **rest
):
    user_id = data["user"]
    try:
        user_response = await web_client.users_info(user=user_id, include_local=True)
        user_name = user_response["user"]["name"]
        print(
            "!",
            await web_client.chat_postMessage(
                channel=data["item"]["channel"],
                text=f"{data['reaction']} is added :{data['reaction']}: by {user_name}",
            ),
        )
    except Exception:
        import traceback

        text = traceback.format_exc()
        channel = os.environ.get("SLACK_DEFAULT_CHANNEL", "#random")
        await web_client.chat_postEphemeral(
            text=text, channel=channel, user=user_id,
        )


async def run_bot():
    loop = asyncio.get_event_loop()
    rtm_client = RTMClient(
        token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop
    )
    await rtm_client.start()


if __name__ == "__main__":
    import logging

    logging.basicConfig(level=logging.DEBUG)
    asyncio.run(run_bot(), debug=True)
