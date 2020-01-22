from handofcats import as_subcommand
import os
import sys
import zmq
import subprocess
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol


@as_subcommand
def run(*, endpoint: str = "tcp://127.0.0.1:5001"):
    sp = subprocess.Popen([sys.executable, __file__, "server", "--endpoint", endpoint])
    cp = subprocess.Popen([sys.executable, __file__, "client", "--endpoint", endpoint])
    cp.wait()
    sp.terminate()
    print("ok")


@as_subcommand
def server(*, endpoint: str):
    import time
    from random import randrange

    context = zmq.Context()
    publisher = context.socket(zmq.PUB)
    publisher.bind(endpoint)

    message_id = 0
    while True:
        zipcode = randrange(10001, 10010)
        temprature = randrange(0, 215) - 80
        relhumidity = randrange(0, 50) + 10

        update = "%05d %d %d %d" % (zipcode, temprature, relhumidity, message_id)
        message_id += 1
        print(os.getpid(), update)
        time.sleep(0.5)
        publisher.send(update.encode("utf-8"))


@as_subcommand
def client(*, endpoint: str, zipfilter="10001 "):
    context = zmq.Context()
    subscriber = context.socket(zmq.SUB)
    subscriber.connect(endpoint)

    # receive only message with zipcode being 10001
    subscriber.setsockopt(zmq.SUBSCRIBE, zipfilter.encode("utf-8"))

    update_samples = 10
    for updates in range(update_samples):
        message = subscriber.recv()
        zipcode, temprature, relhumidity, message_id = message.split()
        print(
            "zip:%s, temp:%s, relh:%s, id:%s"
            % (zipcode, temprature, relhumidity, message_id)
        )
        total_temp = float(temprature)

    print(
        "average temprature for zipcode '%s' was '%f'"
        % (zipfilter, total_temp / update_samples)
    )


as_subcommand.run()
