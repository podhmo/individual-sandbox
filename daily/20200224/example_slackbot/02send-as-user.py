import os
import slacker

token = os.environ["SLACKCLI_API_TOKEN"]
client = slacker.Slacker(token)
response = client.users.list()
for row in response.body["members"]:
    print(row["id"], row["name"])
client.chat.post_ephemeral("#random", "hello", "U02M87T1J")
