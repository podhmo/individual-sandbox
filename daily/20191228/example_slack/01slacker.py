import os
from slacker import Slacker
import dotenv

dotenv.load_dotenv(verbose=True)
token = os.environ["SLACKBOT_API_TOKEN"]
slack = Slacker(token)

# Send a message to #general channel
slack.chat.post_message("#random", "Hello fellow slackers!")

# Get users list
response = slack.users.list()
users = response.body["members"]

# # Upload a file
# slack.files.upload("hello.txt")

# Advanced: Use `request.Session` for connection pooling (reuse)
from requests.sessions import Session

with Session() as session:
    slack = Slacker(token, session=session)
    slack.chat.post_message("#random", "All these requests")
    slack.chat.post_message("#random", "go through")
    slack.chat.post_message("#random", "a single https connection")
