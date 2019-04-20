import requests


def fetch(url: str) -> dict:
    response = requests.get(url)
    return response.json()
