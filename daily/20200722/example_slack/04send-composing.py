import asyncio
import os
from slack import WebClient

client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True)
# type section, text.type plain_text, text.text "hello world" ?
print(f'**{os.environ.get("SLACK_DEFAULT_CHANNEL", "#random")}**')

loop = asyncio.get_event_loop()

payload = {
    "attachments": [
        {
            "blocks": [
                {
                    "text": {"text": "*Alternative hotel options*", "type": "mrkdwn"},
                    "type": "section",
                },
                {
                    "accessory": {
                        "text": {"emoji": True, "text": "View", "type": "plain_text"},
                        "type": "button",
                        "value": "view_alternate_1",
                    },
                    "text": {
                        "text": "<https://example.com|Bates Motel> :star::star:",
                        "type": "mrkdwn",
                    },
                    "type": "section",
                },
                {
                    "accessory": {
                        "text": {"emoji": True, "text": "View", "type": "plain_text"},
                        "type": "button",
                        "value": "view_alternate_2",
                    },
                    "text": {
                        "text": "<https://example.com|The Great Northern Hotel> :star::star::star::star:",
                        "type": "mrkdwn",
                    },
                    "type": "section",
                },
            ]
        }
    ],
    "blocks": [
        {
            "text": {
                "text": "Danny Torrence left the following review for your property:",
                "type": "mrkdwn",
            },
            "type": "section",
        },
        {
            "accessory": {
                "alt_text": "Haunted hotel image",
                "image_url": "https://is5-ssl.mzstatic.com/image/thumb/Purple3/v4/d3/72/5c/d3725c8f-c642-5d69-1904-aa36e4297885/source/256x256bb.jpg",
                "type": "image",
            },
            "block_id": "section567",
            "text": {
                "text": "<https://example.com|Overlook Hotel> \n :star: \n Doors had too many axe holes, guest in room 237 was far too rowdy, whole place felt stuck in the 1920s.",
                "type": "mrkdwn",
            },
            "type": "section",
        },
        {
            "block_id": "section789",
            "fields": [{"text": "*Average Rating*\n1.0", "type": "mrkdwn"}],
            "type": "section",
        },
    ],
    "text": "heh",
}
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
)

payload = {
    "attachments": [
        {
            "fallback": "Plain-text summary of the attachment.",
            "color": "#2eb886",
            "pretext": "Optional text that appears above the attachment block",
            "author_name": "Bobby Tables",
            "author_link": "http://flickr.com/bobby/",
            "author_icon": "http://flickr.com/icons/bobby.jpg",
            "title": "Slack API Documentation",
            "title_link": "https://api.slack.com/",
            "text": "Optional text that appears within the attachment",
            "fields": [{"title": "Priority", "value": "High", "short": False}],
            "image_url": "http://my-website.com/path/to/image.jpg",
            "thumb_url": "http://example.com/path/to/thumb.png",
            "footer": "Slack API",
            "footer_icon": "https://platform.slack-edge.com/img/default_application_icon.png",
        }
    ],
}
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
)

# simplify
payload = {
    "attachments": [
        {
            "color": "#2eb886",
            "pretext": "Optional text that appears above the attachment block",
            "author_name": "Bobby Tables",
            "author_link": "http://flickr.com/bobby/",
            "title": "Slack API Documentation",
            "title_link": "https://api.slack.com/",
            "text": "Optional text that appears within the attachment",
            "fields": [{"title": "Priority", "value": "High", "short": False}],
            "footer": "Slack API",
        }
    ],
}
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
)
