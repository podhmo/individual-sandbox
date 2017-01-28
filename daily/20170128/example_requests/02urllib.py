import urllib.request as r
import http.client
from urllib.request import AbstractHTTPHandler

http.client.HTTPConnection.debuglevel = 1


def __init__(self, debuglevel=None):
    self._debuglevel = debuglevel or self.__class__._debuglevel

AbstractHTTPHandler.__init__ = __init__
AbstractHTTPHandler._debuglevel = 1


response = r.urlopen("http://qiita.com/api/v1/search?q=python")
# print(response.read())
