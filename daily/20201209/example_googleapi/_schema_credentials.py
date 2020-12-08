from typing import List


class Credentials:
    installed: 'Installed'


class Installed:
    auth_provider_x509_cert_url: str
    auth_uri: str
    client_id: str
    client_secret: str
    project_id: str
    redirect_uris: List[str]
    token_uri: str


