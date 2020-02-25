import os
import slacker

token = os.environ["SLACKCLI_API_TOKEN"]
client = slacker.Slacker(token)
client.chat.post_message("#random", "hello", as_user=True)
