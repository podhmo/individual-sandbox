import requests
from dictknife import pp


def run(token, target_url):
    params = {"token": token, "url": target_url, "fields": "meta"}
    response = requests.get("http://api.diffbot.com/v3/article", params)
    response.raise_for_status()
    pp(response.json())


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--token", required=True)
    parser.add_argument("url")
    args = parser.parse_args()
    return run(args.token, args.url)


if __name__ == "__main__":
    main()
