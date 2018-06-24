import contextlib
from epc.client import EPCClient


def main(port):
    client = EPCClient(("localhost", port), log_traceback=True)
    with contextlib.closing(client):

        print("Server provides these methods:")
        print(client.methods_sync())

        print("Calling (add 1 2 3)")
        print("Got: {0}".format(client.call_sync('add', [1, 2, 3])))

        print("Closing client...")
    print("Closing client... Done!")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", required=True, type=int)
    args = parser.parse_args()
    main(args.port)
