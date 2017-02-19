import argparse
import json
from toybox.simpleapi import simple_view, run
from zope.interface import Interface
from pyramid.decorator import reify
from pyramid.httpexceptions import HTTPFound
from rauth import OAuth2Service


class IGithubAuthenticator(Interface):
    # todo: description
    pass


class GithubAuthenticator(object):
    def __init__(self, client_id, client_secret):
        self.client_id = client_id
        self.client_secret = client_secret

    @reify
    def service(self):
        return OAuth2Service(
            client_id=self.client_id,
            client_secret=self.client_secret,
            name='github',
            authorize_url='https://github.com/login/oauth/authorize',
            access_token_url='https://github.com/login/oauth/access_token',
            base_url='https://api.github.com/'
        )

    def get_state_token(self):
        state = "hekeheke"  # todo: using random string
        return state


@simple_view("/callback")
def callback(request):
    github = request.registry.getUtility(IGithubAuthenticator)
    assert request.GET["state"] == github.get_state_token()  # using session

    # get issues
    auth_session = github.service.get_auth_session(data={'code': request.GET["code"]})
    print("access token: ", auth_session.access_token)
    return auth_session.get('/repos/podhmo/toybox/issues').json()


@simple_view("/login")
def login(request):
    github = request.registry.getUtility(IGithubAuthenticator)
    # https://developer.github.com/v3/oauth/#scopes
    authorize_url = github.service.get_authorize_url(scope='repo', state=github.get_state_token())
    raise HTTPFound(location=authorize_url)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config", required=True)
    args = parser.parse_args()
    with open(args.config) as rf:
        data = json.load(rf)

    @run.add_modify
    def modify(config):
        config.registry.registerUtility(GithubAuthenticator(data["client_id"], data["client_secret"]), IGithubAuthenticator)

    run(port=5000)
