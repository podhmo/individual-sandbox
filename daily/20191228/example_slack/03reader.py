import os
import logging
import time
from slackbot.slackclient import SlackClient
import dotenv

logging.basicConfig(level=logging.DEBUG)

dotenv.load_dotenv(verbose=True)
token = os.environ["SLACKBOT_API_TOKEN"]

client = SlackClient(token)
client.rtm_connect()
while True:
    events = client.rtm_read()
    for ev in events:
        print(ev)
    time.sleep(1)
# {"type": "hello"}
# {'type': 'user_typing', 'channel': 'C02M87T2E', 'user': 'U02M87T1J'}
# {'client_msg_id': 'de84cc41-17cb-4cf0-94ae-06ff52a1a8c1', 'suppress_notification': False, 'type': 'message', 'te
# xt': 'hai', 'user': 'U02M87T1J', 'team': 'T02M87T1A', 'blocks': [{'type': 'rich_text', 'block_id': 'QlOL', 'elem
# ents': [{'type': 'rich_text_section', 'elements': [{'type': 'text', 'text': 'hai'}]}]}], 'user_team': 'T02M87T1A
# ', 'source_team': 'T02M87T1A', 'channel': 'C02M87T2E', 'event_ts': '1577500892.003200', 'ts': '1577500892.003200
# '}
# {'type': 'reaction_added', 'user': 'U02M87T1J', 'item': {'type': 'message', 'channel': 'C02M87T2E', 'ts': '15775
# 00892.003200'}, 'reaction': 'cyclone', 'item_user': 'U02M87T1J', 'event_ts': '1577500906.003300', 'ts': '1577500
# 906.003300'}
