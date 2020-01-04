from monogusa.chatbot.slackcli.component import api_client, APIClient


def hello(api_client: APIClient) -> None:
    api_client.send_message("random", "hello")


def repl(api_client: APIClient) -> None:
    breakpoint()
    print("end")


if __name__ == "__main__":
    from monogusa.cli import run

    run()
