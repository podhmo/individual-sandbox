import os
import logging
import pathlib
from handofcats import as_command

import dotenv
from google_auth_oauthlib import flow
from google.oauth2.credentials import Credentials
import gspread

# MEMO: google_auth_oauthlibを使う方法は古い？

@as_command
def run(*, launch_browser: bool = True):
    logging.basicConfig(level=logging.DEBUG)

    scopes = [
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/spreadsheets",
    ]

    dotenv.load_dotenv(verbose=True)

    try:
        credentials = Credentials.from_authorized_user_file(
            str(pathlib.Path(os.environ["GOOGLE_CREDENTIALS"]).expanduser()),
            scopes=scopes,
        )
    except FileNotFoundError:
        json_file = pathlib.Path((os.environ["GOOGLE_SECRET"])).expanduser()

        # TODO: cache
        appflow = flow.InstalledAppFlow.from_client_secrets_file(
            json_file, scopes=scopes
        )

        if launch_browser:
            appflow.run_local_server()
        else:
            appflow.run_console()

        credentials = appflow.credentials
        pathlib.Path(os.environ["GOOGLE_CREDENTIALS"]).expanduser().write_text(
            credentials.to_json()
        )

    from oauth2client.client import OAuth2Credentials

    gclient = gspread.authorize(
        OAuth2Credentials(
            credentials.token,
            credentials.client_id,
            credentials.client_secret,
            credentials.refresh_token,
            credentials.expiry,
            credentials.token_uri,
            "",
            scopes=credentials.scopes,
        )
    )
    try:
        wks = gclient.open("Where is the money Lebowski?").sheet1
    except gspread.SpreadsheetNotFound:
        wks = gclient.create("Where is the money Lebowski?").sheet1
    wks.update_acell('B2', "it's down there somewhere, let me take another look.")

