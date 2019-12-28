import os
import logging
import time
from slackbot.slackclient import SlackClient
import dotenv

logging.basicConfig(level=logging.DEBUG)

dotenv.load_dotenv(verbose=True)
token = os.environ["SLACKBOT_API_TOKEN"]

client = SlackClient(token)
client.ping()

client.send_message("#random", "hello")
client.rtm_send_message("#random", "hello from RTM")

print("hai")
time.sleep(1)
print("hoi")
