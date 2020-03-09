class Client:
    def event(self, fn):
        setattr(self, fn.__name__, fn)
        return fn

    def run(self):
        message = {"msg": "hello"}
        callback = getattr(self, "on_message", None)
        if callback is not None:
            callback(message)


class Reaction:
    def __init__(self, name):
        self.name = name
        self.fns = []

    @property
    def __name__(self):
        return self.name

    def register(self, fn):
        self.fns.append(fn)
        return fn

    def __call__(self, *args, **kwargs):
        for fn in self.fns:
            fn(fn)


class ReactionRouter:
    def __init__(self, setter):
        self.actions = {}
        self.setter = setter

    def register(self, fn):
        name = fn.__name__
        action = self.actions.get(name)
        if action is None:
            action = self.actions[name] = Reaction(name)
            self.setter(action)
        return action.register(fn)


client = Client()
event = ReactionRouter(client.event).register


@event
def on_message(message):
    print("got", message)


@event
def on_message(message):
    print("hmm", message)


client.run()
