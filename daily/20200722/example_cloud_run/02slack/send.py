import os

from slack import WebClient

# Your app's Slack bot user token
SLACK_BOT_TOKEN = os.environ["SLACK_BOT_TOKEN"]
SLACK_VERIFICATION_TOKEN = os.environ["SLACK_VERIFICATION_TOKEN"]

# Slack client for Web API requests
slack_client = WebClient(token=SLACK_BOT_TOKEN)


# Send a Slack message on load. This needs to be _before_ the Flask server is started

# A Dictionary of message attachment options
attachments_json = [
    {
        "fallback": "Upgrade your Slack client to use messages like these.",
        "color": "#3AA3E3",
        "attachment_type": "default",
        "callback_id": "menu_options_2319",
        "actions": [
            {
                "name": "bev_list",
                "text": "Pick a beverage...",
                "type": "select",
                "data_source": "external",
            }
        ],
    }
]

# Send a message with the above attachment, asking the user if they want coffee
slack_client.chat_postMessage(
    channel="#random",
    text="Would you like some coffee? :coffee:",
    attachments=attachments_json,
)
