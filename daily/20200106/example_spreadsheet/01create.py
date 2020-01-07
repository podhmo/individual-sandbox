import os
import logging
import pathlib
from handofcats import as_command

import dotenv
from google_auth_oauthlib import helpers
import gspread

# MEMO: google_auth_oauthlibを使う方法は古い？


class Adapter:
    def __init__(self, session, config):
        self.session = session
        self.config = config

    @property
    def access_token(self):
        self.session.access_token

    def refresh(self, http):
        # http is httplib2.Http
        self.session.refresh_token(self.config["installed"]["token_uri"])


@as_command
def run(*, launch_browser: bool = True):
    logging.basicConfig(level=logging.DEBUG)

    scopes = [
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/spreadsheets",
    ]

    dotenv.load_dotenv(verbose=True)
    json_file = pathlib.Path((os.environ["GOOGLE_SECRET"])).expanduser()
    session, config = helpers.session_from_client_secrets_file(json_file, scopes)
    gclient = gspread.authorize(Adapter(session, config))
