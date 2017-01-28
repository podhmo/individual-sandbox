import getpass
from github import Github
import http.client
from urllib.request import AbstractHTTPHandler

http.client.HTTPConnection.debuglevel = 1


def __init__(self, debuglevel=None):
    self._debuglevel = debuglevel or self.__class__._debuglevel

AbstractHTTPHandler.__init__ = __init__
AbstractHTTPHandler._debuglevel = 1


# First create a Github instance:
name = getpass.getuser()
print("Name(default={})".format(name), end=": ")
name = input() or name
password = getpass.getpass()
g = Github(name, password)

repositories = []
# Then play with your Github objects:
for repo in g.get_user().get_repos():
    repositories.append(repo.name)
print(len(repositories))
