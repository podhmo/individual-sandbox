class Something(object):
    def __init__(self, app=None):
        self.init_app(app)

    def init_app(self, app):
        # configを見て何かする
        self.message = app.config["MESSAGE"]

    def hello(self):
        return self.message
