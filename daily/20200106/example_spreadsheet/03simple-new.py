import logging
import pathlib

from google.oauth2 import service_account
from google.auth.transport.requests import Request


scope = [
    "https://spreadsheets.google.com/feeds",
    "https://www.googleapis.com/auth/drive",
]

logging.basicConfig(level=logging.DEBUG)
# this is deprecated style (oauth2client is deprecated)
credentials = service_account.Credentials.from_service_account_file(
    pathlib.Path("~/Downloads/My Project-746d9dc2ec1f.json").expanduser(), scopes=scope
)
print(credentials.token)
print(credentials.valid)
print(credentials.refresh(Request()))
print(credentials.token)
print(credentials.valid)

