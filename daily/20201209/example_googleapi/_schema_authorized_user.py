from typing import List


class AuthorizedUser:
    client_id: str
    client_secret: str
    refresh_token: str
    scopes: List[str]
    token_uri: str


