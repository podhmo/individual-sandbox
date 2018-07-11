import time
import zmq


def main():
    context = zmq.Context()
    responder = context.socket(zmq.REP)
    rc = responder.bind("tcp://*:55555")

    while True:
        message = responder.recv_string()
        print("recv", message)
        time.sleep(1)  # do something
        responder.send_string("ok")


if __name__ == "__main__":
    main()
