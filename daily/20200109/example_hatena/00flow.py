import dotenv
import os
import requests
from requests_oauthlib import OAuth1
from handofcats import as_command


@as_command
def run():
    # Temporary Credential Request URL
    request_token_uri = "https://www.hatena.com/oauth/initiate"
    # Resource Owner Authorization URL (PC)
    authorize_uri = "https://www.hatena.ne.jp/oauth/authorize"

    dotenv.load_dotenv()
    client_key = os.environ["CLIENT_KEY"]
    client_secret = os.environ["CLIENT_SECRET"]
    # callback_uri = "http://localhost:4567"
    callback_uri = "oob"

    auth = OAuth1(
        client_key,
        client_secret=client_secret,
        callback_uri=callback_uri,
    )
    # session = requests.Session()
    # fetch_response = session.post(
    #     request_token_uri, auth=auth, data={"scope": "read_public,write_public"}
    # )
    fetch_response = requests.post(request_token_uri, auth=auth, data={"scope": "read_public,write_public"})
    print(fetch_response)
    print(fetch_response.content)
