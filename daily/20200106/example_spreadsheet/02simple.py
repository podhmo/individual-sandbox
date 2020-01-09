import logging
import pathlib

import gspread
from oauth2client.service_account import ServiceAccountCredentials


scope = [
    "https://spreadsheets.google.com/feeds",
    "https://www.googleapis.com/auth/drive",
]

logging.basicConfig(level=logging.DEBUG)

# this is deprecated style (oauth2client is deprecated)
credentials = ServiceAccountCredentials.from_json_keyfile_name(
    pathlib.Path("~/Downloads/My Project-746d9dc2ec1f.json").expanduser(), scope
)
print(credentials)
gc = gspread.authorize(credentials)


print(gc.list_spreadsheet_files())
