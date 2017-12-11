import os.path
import httplib2
import argparse
from oauth2client.file import Storage
from oauth2client import client
from oauth2client import tools
from googleapiclient.discovery import build
from dictknife import loading
import threading


class InMemoryCache:
    def __init__(self):
        self.doc = {}

    def set(self, url, doc):
        self.doc[url] = doc

    def get(self, url):
        return self.doc[url]


# https://developers.google.com/analytics/devguides/config/mgmt/v3/quickstart/installed-py?hl=ja
def main(secret_path, credentials_path):
    storage = Storage(credentials_path)
    credentials = storage.get()

    if credentials is None:
        scopes = ['https://www.googleapis.com/auth/analytics.readonly']
        flow = client.flow_from_clientsecrets(secret_path, scopes)
        args = argparse.ArgumentParser(parents=[tools.argparser]).parse_args()
        args.auth_host_port = [44444]
        args.logging_level = "DEBUG"
        credentials = tools.run_flow(flow, storage, args)

    http = credentials.authorize(httplib2.Http())
    print("0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    cache = InMemoryCache()
    resource1 = build("analytics", "v3", http=http, cache_discovery=True, cache=cache)
    print("1@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    resource2 = build("analytics", "v3", http=http, cache_discovery=True, cache=cache)
    print("2@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    accounts = resource1.management().accounts().list().execute()
    loading.dumpfile(accounts, None)
    print("3@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    accounts = resource2.management().accounts().list().execute()
    loading.dumpfile(accounts, None)


if __name__ == "__main__":
    import logging
    logging.basicConfig(level=logging.DEBUG)
    secret_path = os.path.expanduser("~/Downloads/client_secret.json")
    credentials_path = os.path.expanduser("~/.credentials/sample_client2.json")
    main(secret_path, credentials_path)
