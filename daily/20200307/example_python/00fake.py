class Client:
    def event(self, fn):
        setattr(self, fn.__name__, fn)
        return fn

    def run(self):
        message = {"msg": "hello"}
        callback = getattr(self, "on_message", None)
        if callback is not None:
            callback(message)


client = Client()


@client.event
def on_message(message):
    print("got", message)


client.run()
